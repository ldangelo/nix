{ options, config, lib, pkgs, namespace, ... }:
with lib;
with lib.${namespace};
let cfg = config.${namespace}.suites.common;
in {
  options.${namespace}.suites.common = with types; {
    enable = mkBoolOpt false "Whether or not to enable common configuration.";
  };

  config = mkIf cfg.enable {
    programs.zsh = enabled;

    homebrew = {
      brews = [
        "wakatime-cli"
        "alot"
      ];


      casks = [
        "alt-tab"
        "raycast"
        "aldente"
        "hookmark"
        "zoom"
        "microsoft-teams"
        "slack"
        "tradingview"
        "1password"
        "1password-cli"
        "emacs-mac"
        "oh-my-posh"
        "iTerm2"
        "wakatime"
      ];
    };

    environment = {
      loginShell = pkgs.zsh;

      systemPackages = with pkgs; [
        cask
        fasd
        gnugrep
        gnutls
        keychain
        #        pkgs.${namespace}.trace-symlink
        #        pkgs.${namespace}.trace-which
        mas
        moreutils
        terminal-notifier
        trash-cli
        tree
        wtf
        thefuck
      ];
    };

    oftheangels = {
      nix = enabled;

      tools = { homebrew = enabled; };

      system = {
        fonts = enabled;
        input = enabled;
        interface = enabled;
        networking = enabled;
      };
    };
  };
}
