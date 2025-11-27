# modules/home-manager/qutebrowser/keybindings.nix
{ config, pkgs, lib, ... }:

{
  programs.qutebrowser.keyBindings = {
    normal = {
      # Space leader bindings (Spacemacs/Doom style)
      "<Space>p" = "spawn --userscript qute-1pass";
      "<Space>P" = "spawn --userscript qute-1pass --totp";
      "<Space>m" = "spawn mpv {url}";
      "<Space>M" = "hint links spawn mpv {hint-url}";
      "<Space>c" = "spawn --userscript org-capture";
      "<Space>d" = "download";
      "<Space>D" = "hint links download";
      "<Space>b" = "bookmark-add";
      "<Space>q" = "quickmark-save";
      "<Space>Q" = "set-cmd-text -s :quickmark-load";
      "<Space>s" = "set-cmd-text -s :session-save";
      "<Space>l" = "set-cmd-text -s :session-load";
      "<Space>v" = "spawn --userscript view-source";
      "<Space>r" = "spawn --userscript readability";
      "<Space>h" = "help";
      "<Space>/" = "set-cmd-text /";

      # Tab navigation (vim-style)
      "J" = "tab-prev";
      "K" = "tab-next";
      "x" = "tab-close";
      "X" = "undo";

      # History navigation
      "H" = "back";
      "L" = "forward";
    };

    insert = {
      # Emacs-style editing
      "<Ctrl-a>" = "fake-key <Home>";
      "<Ctrl-e>" = "fake-key <End>";
      "<Ctrl-h>" = "fake-key <Backspace>";
      "<Ctrl-w>" = "fake-key <Ctrl-Backspace>";
      "<Ctrl-u>" = "fake-key <Shift-Home><Delete>";
      "<Ctrl-k>" = "fake-key <Shift-End><Delete>";
      "<Ctrl-[>" = "mode-leave";
    };

    command = {
      "<Ctrl-a>" = "rl-beginning-of-line";
      "<Ctrl-e>" = "rl-end-of-line";
      "<Ctrl-[>" = "mode-leave";
    };

    prompt = {
      "<Ctrl-a>" = "rl-beginning-of-line";
      "<Ctrl-e>" = "rl-end-of-line";
      "<Ctrl-[>" = "mode-leave";
    };
  };

  # Key mappings (global remaps)
  programs.qutebrowser.keyMappings = {
    "<Ctrl-[>" = "<Escape>";
  };
}
