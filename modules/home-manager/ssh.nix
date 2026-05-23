{ config, lib, pkgs, ... }:

let
  homeDir = config.home.homeDirectory;
  secretsFile = ../../secrets/secrets.yaml;
  secretsText = builtins.readFile secretsFile;
  hasSshPrivateKey = lib.hasInfix "id_ed25519:" secretsText;
  hasSshPublicKey = lib.hasInfix "id_ed25519_pub:" secretsText;
in {
  services.ssh-agent = {
    enable = true;
    package = pkgs.openssh;
  };

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks."*" = {
      forwardAgent = false;
      addKeysToAgent = "yes";
      compression = false;
      serverAliveInterval = 0;
      serverAliveCountMax = 3;
      hashKnownHosts = false;
      userKnownHostsFile = "~/.ssh/known_hosts";
      controlMaster = "no";
      controlPath = "~/.ssh/master-%r@%n:%p";
      controlPersist = "no";
      identityFile = lib.mkIf hasSshPrivateKey [ "${homeDir}/.ssh/id_ed25519" ];
    };

    matchBlocks."tailscale" = lib.mkIf hasSshPrivateKey {
      host = "*.ts.net 100.*";
      user = "ldangelo";
      identitiesOnly = true;
      identityFile = [ "${homeDir}/.ssh/id_ed25519" ];
      extraOptions.StrictHostKeyChecking = "accept-new";
    };
  };

  home.activation.createSshDirectory = lib.hm.dag.entryBefore [ "sops-nix" ] ''
    mkdir -p "$HOME/.ssh"
    chmod 700 "$HOME/.ssh"
  '';

  home.activation.installManagedAuthorizedKey = lib.mkIf hasSshPublicKey (
    lib.hm.dag.entryAfter [ "sops-nix" ] ''
      key_file="$HOME/.ssh/id_ed25519.pub"
      auth_file="$HOME/.ssh/authorized_keys"
      begin_marker="# nix-managed shared ssh key begin"
      end_marker="# nix-managed shared ssh key end"

      if [ -s "$key_file" ]; then
        tmp_file="$(mktemp)"
        if [ -f "$auth_file" ]; then
          awk -v begin="$begin_marker" -v end="$end_marker" '
            $0 == begin { skip = 1; next }
            $0 == end { skip = 0; next }
            skip != 1 { print }
          ' "$auth_file" > "$tmp_file"
        fi

        {
          cat "$tmp_file"
          printf "%s\n" "$begin_marker"
          cat "$key_file"
          printf "\n%s\n" "$end_marker"
        } > "$auth_file"

        rm -f "$tmp_file"
        chmod 600 "$auth_file"
      fi
    ''
  );

  sops = {
    defaultSopsFile = secretsFile;
    age.keyFile = "${homeDir}/.config/sops/age/keys.txt";
    age.sshKeyPaths = [ "${homeDir}/.ssh/id_ed25519" ];
    gnupg.sshKeyPaths = [];

    secrets = lib.mkMerge [
      (lib.mkIf hasSshPrivateKey {
        "ssh/id_ed25519" = {
          key = "ssh/id_ed25519";
          path = "${homeDir}/.ssh/id_ed25519";
          mode = "0400";
        };
      })
      (lib.mkIf hasSshPublicKey {
        "ssh/id_ed25519_pub" = {
          key = "ssh/id_ed25519_pub";
          path = "${homeDir}/.ssh/id_ed25519.pub";
          mode = "0444";
        };
      })
    ];
  };
}
