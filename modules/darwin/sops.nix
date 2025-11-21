{ config, pkgs, ... }:

{
  sops.gnupg.sshKeyPaths = [];

  # Point to the default secrets file
  sops.defaultSopsFile = ../../secrets/secrets.yaml;

  # Use age with SSH keys
  sops.age.keyFile = "/Users/ldangelo/.config/sops/age/keys.txt";

  # Also support SSH keys for decryption
  sops.age.sshKeyPaths = [ "/Users/ldangelo/.ssh/id_ed25519" ];

  # Secrets configuration
  sops.secrets."github/token" = {
    owner = "ldangelo";
    mode = "0400";
  };

  sops.secrets.anthropic_api_key = {
    owner = "ldangelo";
    mode = "0400";
  };
}
