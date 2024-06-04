{ config, lib, namespace, ... }:
let
  inherit (lib) mkForce;
  inherit (lib.${namespace}) enabled disabled;
in {

  oftheangels = {
    user = {
      enable = true;
      inherit (config.snowfallorg.user) name;

      email = enabled;
    };

    
    cli-apps = { neovim = enabled; };
    services = { };

    programs = {
      terminal = {
        shells = { zsh = enabled; };

        tools = {
          tmux = enabled;
          git = enabled;
          direnv = enabled;
          home-manager = enabled;
        };
      };
    };

    services = {
      sops = {
        enable = true;
        defaultSopsFile = lib.snowfall.fs.get-file "secrets/secrets.yaml";
        sshKeyPaths = [ "${config.home.homeDirectory}/.ssh/id_ed25519" ];
      };
    };

    suites = { 
    common = enabled;
    development = enabled;
    };
  };

  #   home.sessionPath = [ "$HOME/bin" ];

  home.stateVersion = "22.11";
}
