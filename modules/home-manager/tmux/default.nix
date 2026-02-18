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
      {
        plugin = tmux-thumbs;
        extraConfig = ''
          set -g @thumbs-key F
        '';
      }
      fzf-tmux-url
      extrakto
      {
        plugin = tmux-sessionx;
        extraConfig = ''
          set -g @sessionx-bind 'o'
          set -g @sessionx-filter-current 'false'
          set -g @sessionx-preview-enabled 'true'
        '';
      }
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
        plugin = tmux-which-key;
        extraConfig = ''
          set -g @tmux-which-key-xdg-enable 1
          set -g @tmux-which-key-disable-autobuild 1
        '';
      }
      {
        plugin = catppuccin;
        extraConfig = ''
          set -g @catppuccin_flavor "mocha"
          set -g @catppuccin_window_status_style "slanted"
          set -g @catppuccin_window_default_text "#W"
          set -g @catppuccin_window_current_text "#W"
        '';
      }
    ];

    extraConfig = ''
      # Status line — must come after catppuccin plugin sets up variables.
      set -g status-right-length 100
      set -g status-left-length 100
      set -g status-left ""
      set -g status-right "#{E:@catppuccin_status_session}"
      set -ag status-right "#{E:@catppuccin_status_date_time}"

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
      bind -T copy-mode-vi C-v send-keys -X rectangle-toggle
      bind -T copy-mode-vi H send-keys -X start-of-line
      bind -T copy-mode-vi L send-keys -X end-of-line
      bind Enter copy-mode

      # Window navigation
      bind Tab last-window
      bind BTab switch-client -l

      # Window reordering
      bind -r "<" swap-window -d -t -1
      bind -r ">" swap-window -d -t +1

      # Mouse toggle
      bind m set -g mouse \; display "Mouse: #{?mouse,ON,OFF}"

      # Activity monitoring (highlight windows with new output)
      set -g monitor-activity on
      set -g visual-activity off

      # Reload config
      bind r source-file ~/.config/tmux/tmux.conf \; display "Config reloaded"

      # Switch sessions
      bind S choose-session

      # Focus events for Neovim autoread
      set -g focus-events on

      # Override vim-tmux-navigator bindings to pass keys through to apps that
      # need them (Neovim, fzf). Claude Code is excluded — it treats C-h as
      # backspace, so we let tmux handle pane navigation instead.
      is_passthrough="ps -o state= -o comm= -t '#{pane_tty}' | grep -iqE '^[^TXZ ]+ +(\\S+/)?g?\\.?(view|l?n?vim?x?|fzf|sidecar)(diff)?(-wrapped)?$'"
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
  # tmux-which-key configuration
  xdg.configFile."tmux-which-key/config.yaml".text = ''
    command_alias_start_index: 200
    keybindings:
      prefix_table: Space
    title:
      style: align=centre,bold
      prefix: tmux
      prefix_style: fg=green,align=centre,bold
    position:
      x: C
      y: S
    macros:
      - name: reload-config
        commands:
          - source-file ~/.config/tmux/tmux.conf
          - display "Config reloaded"
    items:
      - name: Run
        key: space
        command: command-prompt
      - name: Last window
        key: tab
        command: last-window
      - separator: true
      - name: +Windows
        key: w
        menu:
          - name: Last
            key: tab
            command: last-window
          - name: Choose
            key: w
            command: choose-tree -Zw
          - name: Previous
            key: p
            command: previous-window
          - name: Next
            key: n
            command: next-window
          - name: New
            key: c
            command: "neww -c #{pane_current_path}"
          - separator: true
          - name: Split |
            key: /
            command: "splitw -h -c #{pane_current_path}"
          - name: Split -
            key: "-"
            command: "splitw -v -c #{pane_current_path}"
          - separator: true
          - name: Rename
            key: R
            command: command-prompt -I "#W" "renamew -- \"%%\""
          - name: Kill
            key: X
            command: confirm -p "Kill window #W? (y/n)" killw
      - name: +Panes
        key: p
        menu:
          - name: Last
            key: tab
            command: lastp
          - name: Choose
            key: p
            command: displayp -d 0
          - separator: true
          - name: Zoom
            key: z
            command: resizep -Z
          - name: +Resize
            key: r
            menu:
              - name: Left
                key: h
                command: resizep -L 5
                transient: true
              - name: Down
                key: j
                command: resizep -D 5
                transient: true
              - name: Up
                key: k
                command: resizep -U 5
                transient: true
              - name: Right
                key: l
                command: resizep -R 5
                transient: true
          - separator: true
          - name: Break to window
            key: "!"
            command: break-pane
          - name: Kill
            key: X
            command: confirm -p "Kill pane #P? (y/n)" killp
          - name: Sync panes
            key: "Y"
            command: setw synchronize-panes
      - name: +Sessions
        key: s
        menu:
          - name: Choose
            key: s
            command: choose-tree -Zs
          - name: Sessionx
            key: x
            command: "display 'Use Prefix+o for sessionx'"
          - name: New
            key: N
            command: new
          - name: Rename
            key: r
            command: rename
          - name: Detach
            key: d
            command: detach
      - name: Copy mode
        key: c
        command: copy-mode
      - separator: true
      - name: +Popups
        key: t
        menu:
          - name: Shell
            key: t
            command: "run \"#{@popup-toggle} -w75% -h75% -Ed'#{pane_current_path}'\""
          - name: Lazygit
            key: g
            command: "run \"#{@popup-toggle} -w90% -h90% -Ed'#{pane_current_path}' --name=lazygit lazygit\""
          - name: Deploy
            key: d
            command: "run \"#{@popup-toggle} -w80% -h60% -Ed'#{pane_current_path}' --name=sudo -H deploy just deploy\""
          - name: Help
            key: h
            command: "run \"#{@popup-toggle} -w90% -h90% --name=help glow -p ${../../../docs/tmux-guide.md}\""
      - separator: true
      - name: Reload config
        key: R
        macro: reload-config
      - name: Keys
        key: "?"
        command: list-keys -N
  '';

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
            - nvim 
  '';

  xdg.configFile."tmuxinator/dev.yml".text = ''
    name: dev
    root: .
    windows:
      - claude:
          layout: even-horizontal
          panes:
            - nvim 
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
            - nvim 
            - claude --continue
  '';

  xdg.configFile."tmuxinator/notes.yml".text = ''
    name: notes
    root: ~/Library/Mobile Documents/iCloud~md~obsidian/Documents/ldangelo
    windows:
      - editor:
          panes:
            - nvim .
      - shell:
          panes:
            - ""
  '';

  xdg.configFile."tmuxinator/ops.yml".text = ''
    name: ops
    root: .
    windows:
      - shell:
          panes:
            - ""
      - logs:
          panes:
            - ""
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
