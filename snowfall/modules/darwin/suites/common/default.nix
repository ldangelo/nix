{ options, config, lib, pkgs, namespace, ... }:
with lib;
with lib.${namespace};
let cfg = config.${namespace}.suites.common;
in {
  options.${namespace}.suites.common = with types; {
    enable = mkBoolOpt false "Whether or not to enable common configuration.";
  };

  config = mkIf cfg.enable {
    # this sets zsh as the default shell for the user
    programs.zsh.enable = true;

    homebrew = {
      brews = [ "wakatime-cli" "alot" "libvterm" "libtool" "vfkit" ];

      casks = [
        "1password"
        "1password-cli"
        "aldente"
        "alt-tab"
        "emacs-mac"
        "hammerspoon"
        "hookmark"
        "iTerm2"
        "mactex"
        "microsoft-teams"
        "oh-my-posh"
        "raycast"
        "slack"
        "tradingview"
        "wakatime"
        "wezterm"
        "zoom"
        "obsidian"
      ];
    };

    environment = {
      loginShell = pkgs.zsh;

      systemPackages = with pkgs; [
        ollama
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

      tools = {
        homebrew = enabled;
        podman = enabled;
      };

      system = {
        fonts = enabled;
        input = enabled;
        interface = enabled;
        networking = enabled;
      };
    };
  };
}
