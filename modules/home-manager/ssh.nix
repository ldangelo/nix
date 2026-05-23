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
  };

  home.activation.createSshDirectory = lib.hm.dag.entryBefore [ "sops-nix" ] ''
    mkdir -p "$HOME/.ssh"
    chmod 700 "$HOME/.ssh"
  '';

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
