{ config, pkgs, ... }:

{
  home.stateVersion = "24.05";
  home.packages = [ pkgs.direnv ];

  # Create .envrc file that reads from sops secrets
  home.file.".envrc".text = ''
    export ANTHROPIC_API_KEY="$(cat /run/secrets/anthropic_api_key)"
    export OPENROUTER_API_KEY="$(cat /run/secrets/openrouter_api_key)"
    export SOPS_AGE_KEY_FIlE=~/.config/sops/age/keys.txt
  '';
}

