{ config, ... }:

let
  homeDir = config.home.homeDirectory;
in
{
  sops.gnupg.sshKeyPaths = [];
  sops.defaultSopsFile = ../../secrets/secrets.yaml;
  sops.age.keyFile = "${homeDir}/.config/sops/age/keys.txt";
  sops.age.sshKeyPaths = [ "${homeDir}/.ssh/id_ed25519" ];

  sops.secrets."github/token".mode = "0400";
  sops.secrets."github/fortium".mode = "0400";
  sops.secrets.anthropic_api_key.mode = "0400";
  sops.secrets.openrouter_api_key.mode = "0400";
  sops.secrets.mac_mail_key.mode = "0400";
  sops.secrets.openclaw_gateway_token.mode = "0400";
  sops.secrets.openai_api_key.mode = "0400";
  sops.secrets.minimax_api_key.mode = "0400";
  sops.secrets.litellm_ui_password.mode = "0400";
  sops.secrets.litellm_master_key.mode = "0400";
}
