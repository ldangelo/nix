{ config, pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    tmuxinator.enable = true;
    prefix = "C-Space";
    mouse = true;
    terminal = "tmux-256color";
    baseIndex = 1;
    escapeTime = 0;
    historyLimit = 50000;
    keyMode = "vi";
    sensibleOnTop = true;

    plugins = with pkgs.tmuxPlugins; [
      vim-tmux-navigator
      yank
      {
        plugin = catppuccin;
        extraConfig = ''
          set -g @catppuccin_flavor "mocha"
          set -g @catppuccin_window_status_style "rounded"
          set -g @catppuccin_window_default_text "#W"
          set -g @catppuccin_window_current_text "#W"
          set -g @catppuccin_status_modules_right "session date_time"
        '';
      }
      {
        plugin = pkgs.tmuxPlugins.mkTmuxPlugin {
          pluginName = "command-palette";
          version = "unstable-2025-01-01";
          src = pkgs.fetchFromGitHub {
            owner = "lost-melody";
            repo = "tmux-command-palette";
            rev = "main";
            hash = "sha256-zkQhWRd4AiH9XjfRausvB2MNR3xXirYJA13OtvQ4oGQ=";
          };
        };
        extraConfig = ''
          set -g @cmdpalette-key-prefix 'prefix ?'
          set -g @cmdpalette-key-root 'prefix BSpace'
        '';
      }
    ];

    extraConfig = ''
      # True color support
      set -ag terminal-overrides ",xterm-256color:RGB"
      set -ag terminal-overrides ",ghostty:RGB"

      # Renumber windows when one is closed
      set -g renumber-windows on

      # Split panes with | and -
      bind | split-window -h -c "#{pane_current_path}"
      bind - split-window -v -c "#{pane_current_path}"

      # New window in current path
      bind c new-window -c "#{pane_current_path}"

      # Vi-style pane resizing
      bind -r H resize-pane -L 5
      bind -r J resize-pane -D 5
      bind -r K resize-pane -U 5
      bind -r L resize-pane -R 5

      # Vi-style copy mode
      bind -T copy-mode-vi v send-keys -X begin-selection
      bind -T copy-mode-vi y send-keys -X copy-pipe-and-cancel "pbcopy"

      # Reload config
      bind r source-file ~/.config/tmux/tmux.conf \; display "Config reloaded"

      # Switch sessions
      bind S choose-session

      # Pane zoom toggle (already prefix+z by default)

      # Focus events for Neovim autoread
      set -g focus-events on

      # Override vim-tmux-navigator bindings to pass keys through to apps that
      # need them. Sidecar runs claude/bash in the outer pane, so we match those
      # process names too. The pane_current_command check is faster than ps.
      is_passthrough="ps -o state= -o comm= -t '#{pane_tty}' | grep -iqE '^[^TXZ ]+ +(\\S+/)?g?\\.?(view|l?n?vim?x?|fzf|sidecar|claude|claude-code)(diff)?(-wrapped)?$'"
      bind-key -T root C-h  if-shell "$is_passthrough" "send-keys C-h"  "select-pane -L"
      bind-key -T root C-j  if-shell "$is_passthrough" "send-keys C-j"  "select-pane -D"
      bind-key -T root C-k  if-shell "$is_passthrough" "send-keys C-k"  "select-pane -U"
      bind-key -T root C-l  if-shell "$is_passthrough" "send-keys C-l"  "select-pane -R"
      bind-key -T root C-\\ if-shell "$is_passthrough" "send-keys C-\\\\" "select-pane -l"
    '';
  };

  # Tmuxinator project: dev workspace (lazygit, yazi, nvim+claude)
  xdg.configFile."tmuxinator/dev.yml".text = ''
    name: dev
    root: .
    windows:
      - claude:
          layout: even-horizontal
          panes:
            - nvim .
            - claude --continue
      - tasks:
          panes:
            - td monitor .
      - git:
          panes:
            - lazyjj .
      - files:
          panes:
            - yazi .
  '';

  # sudo askpass helper — shows macOS GUI dialog when no TTY is available
  home.file.".local/bin/sudo-askpass" = {
    executable = true;
    text = ''
      #!/bin/bash
      /usr/bin/osascript -e 'display dialog "sudo password:" default answer "" with hidden answer with title "sudo"' -e 'text returned of result' 2>/dev/null
    '';
  };

  # Deploy layout scripts
  home.file.".local/bin/tmux-simple" = {
    source = ./layouts/simple.sh;
    executable = true;
  };
  home.file.".local/bin/tmux-editor" = {
    source = ./layouts/editor.sh;
    executable = true;
  };
  home.file.".local/bin/tmux-dev" = {
    source = ./layouts/dev.sh;
    executable = true;
  };
  home.file.".local/bin/tmux-monitor" = {
    source = ./layouts/monitor.sh;
    executable = true;
  };
  home.file.".local/bin/tmux-claude" = {
    source = ./layouts/claude.sh;
    executable = true;
  };
}
