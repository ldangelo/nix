{ config, pkgs, ... }:

{
  programs.tmux = {
    enable = true;
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
