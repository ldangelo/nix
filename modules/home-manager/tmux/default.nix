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
      {
        plugin = vim-tmux-navigator;
        extraConfig = ''
          # Ensure Nix paths are available to plugin run-shell commands
          set-environment -g PATH "/etc/profiles/per-user/ldangelo/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
        '';
      }
      yank
      {
        plugin = resurrect;
        extraConfig = ''
          set -g @resurrect-capture-pane-contents 'on'
          set -g @resurrect-strategy-nvim 'session'
        '';
      }
      {
        plugin = continuum;
        extraConfig = ''
          set -g @continuum-restore 'on'
          set -g @continuum-save-interval '10'
        '';
      }
      tmux-thumbs
      fzf-tmux-url
      extrakto
      {
        plugin = tmux-toggle-popup;
        extraConfig = ''
          set -g @popup-autostart on
          # Prefix t — general popup shell (75% size)
          bind t run "#{@popup-toggle} -w75% -h75% -Ed'#{pane_current_path}'"
          # Prefix g — lazygit popup (90% size)
          bind g run "#{@popup-toggle} -w90% -h90% -Ed'#{pane_current_path}' --name=lazygit lazygit"
          # Prefix D — deploy popup (just deploy)
          bind D run "#{@popup-toggle} -w80% -h60% -Ed'#{pane_current_path}' --name=deploy just deploy"
          # Prefix h — tmux user guide
          bind h run "#{@popup-toggle} -w90% -h90% --name=help glow -p ${../../../docs/tmux-guide.md}"
        '';
      }
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
          # macOS BSD getopt lacks long option support; patch to use GNU getopt
          postInstall = ''
            substituteInPlace $target/scripts/env.sh \
              --replace-fail 'getopt' '${pkgs.getopt}/bin/getopt'
          '';
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

      # UX tweaks
      set -g display-time 2000
      set -g detach-on-destroy off
      set -g pane-border-status top
      set -g pane-border-format " #{pane_index}: #{pane_current_command} "
    '';
  };

  # Tmuxinator workspaces
  xdg.configFile."tmuxinator/simple.yml".text = ''
    name: simple
    root: .
    windows:
      - shell:
          panes:
            - ""
  '';

  xdg.configFile."tmuxinator/editor.yml".text = ''
    name: editor
    root: .
    windows:
      - editor:
          panes:
            - nvim .
  '';

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

  xdg.configFile."tmuxinator/monitor.yml".text = ''
    name: monitor
    root: .
    windows:
      - dashboard:
          layout: tiled
          panes:
            - htop
            - watch -n 2 df -h
            - watch -n 2 netstat -an
            - ""
  '';

  xdg.configFile."tmuxinator/claude.yml".text = ''
    name: claude
    root: .
    windows:
      - pair:
          layout: even-horizontal
          panes:
            - nvim .
            - claude --continue
  '';

  # sudo askpass helper — shows macOS GUI dialog when no TTY is available
  home.file.".local/bin/sudo-askpass" = {
    executable = true;
    text = ''
      #!/bin/bash
      /usr/bin/osascript -e 'display dialog "sudo password:" default answer "" with hidden answer with title "sudo"' -e 'text returned of result' 2>/dev/null
    '';
  };
}
