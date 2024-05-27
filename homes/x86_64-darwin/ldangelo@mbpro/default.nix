{
  lib,
  pkgs,
  config,
  osConfig ? {},
  format ? "unknown",
  namespace,
  ...
}:
with lib.${namespace}; {
  oftheangels = {
    user = {
      enable = true;
      name = config.oftheangels.user.name;
    };

    cli-apps = {
      zsh = enabled;
      tmux = enabled;
      neovim = enabled;
      home-manager = enabled;
    };

    tools = {
      git = enabled;
      direnv = enabled;
    };
  };

  home.sessionPath = [
    "$HOME/bin"
  ];

  home.stateVersion = "22.11";
}
