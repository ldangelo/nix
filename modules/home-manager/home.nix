{ config, pkgs, ... }:

{
  sops.secrets.anthropic_api_key = {
    sopsFile = ../secrets.yaml;
  };

  home.file.".envrc".text = ''
    export ANTHROPIC_API_KEY="$(cat ${config.sops.secrets.anthropic_api_key.path})"
  '';

  home.packages = [ pkgs.direnv ];
}

