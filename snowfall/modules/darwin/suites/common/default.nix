{
  options,
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.suites.common;
in {
  options.${namespace}.suites.common = with types; {
    enable = mkBoolOpt false "Whether or not to enable common configuration.";
  };

  config = mkIf cfg.enable {
    programs.zsh = enabled;

    oftheangels = {
      nix = enabled;

      apps = {
        kitty = enabled;
        alacritty = enabled;
        spacemacs = enabled;
        raycast = enabled;
      };

      cli-apps = {
        neovim = enabled;
      };

      tools = {
        git = enabled;
        flake = enabled;
      };

      system = {
        fonts = enabled;
      };

      security = {
        gpg = enabled;
      };
    };
  };
}
