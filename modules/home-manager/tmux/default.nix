{ config, lib, pkgs, ... }:

let
  # tmux-notify: monitors panes and sends macOS notifications when processes finish
  # Not in nixpkgs — built from source (rickstaa/tmux-notify)
  tmux-notify = pkgs.tmuxPlugins.mkTmuxPlugin {
    pluginName = "tmux-notify";
    version = "unstable-2026-03-05";
    src = pkgs.fetchFromGitHub {
      owner = "rickstaa";
      repo = "tmux-notify";
      rev = "b713320af05837c3b44e4d51167ff3062dbeae4b";
      sha256 = "sha256-wOmq2stWXAFmYrRuIqf9IPATYXJ+OFoYXnJdHUnJQxY=";
    };
  };

  # treemux: Nvim-Tree/Neo-Tree file explorer as a tmux sidebar
  # Not in nixpkgs — built from source (kiyoon/treemux)
  treemux = pkgs.tmuxPlugins.mkTmuxPlugin {
    pluginName = "treemux";
    version = "unstable";
    src = pkgs.fetchFromGitHub {
      owner = "kiyoon";
      repo = "treemux";
      rev = "bfa5ea669aff98777dcbc7ed871cd859ef81c5ef";
      sha256 = "sha256-1mCxTv3KqUsCjeI7X02NBMRJJzbL0cE1Gg20FrMDChI=";
    };
  };

  # tmux-tea: fuzzy tmux session manager (zoxide, fzf, tmuxinator)
  # Not in nixpkgs — built from source (2KAbhishek/tmux-tea)
  tmux-tea = pkgs.tmuxPlugins.mkTmuxPlugin {
    pluginName = "tmux-tea";
    version = "unstable";
    src = pkgs.fetchFromGitHub {
      owner = "2KAbhishek";
      repo = "tmux-tea";
      rev = "806aa7186c0344e0c7b2c9fa0c044267d6b3ca9e";
      sha256 = "sha256-Z5IaZG4OJUqERz1P8aZu0CVcuo4v741rqTob9HBaqU8=";
    };
  };

  # tmux-fzf: fzf-based session/window/pane/command/keybinding/clipboard/process manager
  # Not in nixpkgs — built from source (sainnhe/tmux-fzf)
  tmux-fzf = pkgs.tmuxPlugins.mkTmuxPlugin {
    pluginName = "tmux-fzf";
    version = "unstable";
    src = pkgs.fetchFromGitHub {
      owner = "sainnhe";
      repo = "tmux-fzf";
      rev = "05af76daa2487575b93a4f604693b00969f19c2f";
      sha256 = "sha256-ay7z0MkeDCpxdwNTKFrkxi/hUE7a5K7P7oFhfn94aLA=";
    };
  };
in
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
          set-environment -g PATH "/etc/profiles/per-user/ldangelo/bin:/nix/var/nix/profiles/system/sw/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
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
        plugin = tmux-toggle-popup;
        extraConfig = ''
          set -g @popup-autostart on
          # Prefix t — general popup shell (75% size)
          bind t run "#{@popup-toggle} -w75% -h75% -Ed'#{pane_current_path}'"
          # Prefix g — lazygit popup (90% size)
          bind g run "#{@popup-toggle} -w90% -h90% -Ed'#{pane_current_path}' --name=lazygit lazygit"
          # Prefix y — yazi file browser popup (90% size)
          bind y run "#{@popup-toggle} -w90% -h90% -Ed'#{pane_current_path}' --name=yazi yazi"
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
      {
        plugin = cpu;
        extraConfig = ''
          set -g @cpu_low_icon "▁"
          set -g @cpu_medium_icon "▄"
          set -g @cpu_high_icon "█"
          set -g @cpu_percentage_format "%3.1f%%"
        '';
      }
      {
        plugin = battery;
        extraConfig = ''
          set -g @batt_icon_charge_tier8 "█"
          set -g @batt_icon_charge_tier7 "▇"
          set -g @batt_icon_charge_tier6 "▆"
          set -g @batt_icon_charge_tier5 "▅"
          set -g @batt_icon_charge_tier4 "▄"
          set -g @batt_icon_charge_tier3 "▃"
          set -g @batt_icon_charge_tier2 "▂"
          set -g @batt_icon_charge_tier1 "▁"
          set -g @batt_icon_status_charged "⚡"
          set -g @batt_icon_status_charging "↑"
          set -g @batt_icon_status_discharging ""
          set -g @batt_color_status_primary_charged "#a6e3a1"
          set -g @batt_color_status_primary_charging "#f9e2af"
          set -g @batt_color_status_primary_discharging "#cdd6f4"
        '';
      }
    ];
    
    extraConfig = ''
      # tmux-fzf: fzf-based session/window/pane/command/keybinding/clipboard/process manager
      # Note: plugin uses main.tmux instead of tmux_fzf.tmux
      set -g @tmux-fzf-launch-key-assign "F"
      set -g @tmux-fzf-preview-enabled "true"
      set -g @tmux-fzf-popup-enable "true"
      run-shell "${tmux-fzf}/share/tmux-plugins/tmux-fzf/main.tmux"

      # tmux-tea: fuzzy tmux session manager
      # Note: plugin uses tea.tmux instead of tmux_tea.tmux
      set -g @tea-bind 'o'
      set -g @tea-default-command 'nvim .'
      set -g @tea-find-path "$HOME/Development"
      set -g @tea-preview-position 'top'
      set -g @tea-session-name 'basename'
      run-shell "${tmux-tea}/share/tmux-plugins/tmux-tea/tea.tmux"

      # treemux: Nvim-Tree/Neo-Tree file explorer as a tmux sidebar
      # Note: plugin uses sidebar.tmux instead of treemux.tmux
      set -g @treemux-tree-client 'nvim-tree'
      set -g @treemux-tree-nvim-init-file "$HOME/.local/share/tmux/plugins/treemux_init.lua"
      set -g @treemux-nvim-command 'NVIM_APPNAME=nvim-treemux nvim'
      run-shell "${treemux}/share/tmux-plugins/treemux/sidebar.tmux"

      # tmux-notify: monitors panes and sends macOS notifications when processes finish
      # Note: plugin uses tnotify.tmux instead of tmux_notify.tmux
      set -g @tnotify-verbose 'on'
      set -g @tnotify-sleep-duration '5'
      set -g @tnotify-verbose-msg '#S:#W — process finished'
      run-shell "${tmux-notify}/share/tmux-plugins/tmux-notify/tnotify.tmux"

      # Extended keys — required for modified Enter/Tab keys (e.g. Pi, Neovim)
      # Pi expects CSI-u encoding; xterm format causes keybinding warnings.
      set -g extended-keys on
      set -g extended-keys-format csi-u

      # Status line — must come after catppuccin plugin sets up variables.
      set -g status-right-length 150
      set -g status-left-length 100
      set -g status-left ""
      set -g status-right "#{E:@catppuccin_status_session}"
      set -ag status-right " #[fg=#89b4fa] #{cpu_icon} #{cpu_percentage}"
      set -ag status-right " #[fg=#{@batt_color_status_primary}]#{batt_icon_status} #{batt_percentage}"
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
      bind v copy-mode

      # Session/project navigation
      bind f run-shell '~/.local/bin/tmux-project-picker'
      bind S choose-tree -Zs
      bind N new-session -c "#{pane_current_path}"
      bind-key -n M-t run-shell '~/.local/bin/tmux-template "#{pane_current_path}"'
      bind-key -n M-1 run-shell 'target="$(tmux list-sessions -F "##{session_id}" | sed -n "1p")" && tmux switch-client -t "$target"'
      bind-key -n M-2 run-shell 'target="$(tmux list-sessions -F "##{session_id}" | sed -n "2p")" && tmux switch-client -t "$target"'
      bind-key -n M-3 run-shell 'target="$(tmux list-sessions -F "##{session_id}" | sed -n "3p")" && tmux switch-client -t "$target"'
      bind-key -n M-4 run-shell 'target="$(tmux list-sessions -F "##{session_id}" | sed -n "4p")" && tmux switch-client -t "$target"'
      bind-key -n M-5 run-shell 'target="$(tmux list-sessions -F "##{session_id}" | sed -n "5p")" && tmux switch-client -t "$target"'
      bind-key -n M-6 run-shell 'target="$(tmux list-sessions -F "##{session_id}" | sed -n "6p")" && tmux switch-client -t "$target"'
      bind-key -n M-7 run-shell 'target="$(tmux list-sessions -F "##{session_id}" | sed -n "7p")" && tmux switch-client -t "$target"'
      bind-key -n M-8 run-shell 'target="$(tmux list-sessions -F "##{session_id}" | sed -n "8p")" && tmux switch-client -t "$target"'
      bind-key -n M-9 run-shell 'target="$(tmux list-sessions -F "##{session_id}" | sed -n "9p")" && tmux switch-client -t "$target"'

      # Auto-name newly created sessions after their starting directory.
      set-hook -g session-created 'run-shell -b "$HOME/.local/bin/tmux-auto-rename-session #{q:session_id}"'

      # Window navigation
      bind Tab last-window
      bind BTab switch-client -l

      # Window reordering
      bind -r "<" swap-window -d -t -1
      bind -r ">" swap-window -d -t +1

      # Mouse toggle
      bind m set -g mouse \; display "Mouse: #{?mouse,ON,OFF}"
      # Reload config
      bind r source-file ~/.config/tmux/tmux.conf \; display "Config reloaded"
      # Activity monitoring
      set -g visual-activity off

      # Bell monitoring — Claude Code and other agents send terminal bells
      # when waiting for input. This highlights the window and triggers
      # a macOS notification via terminal-notifier.
      set -g monitor-bell on
      set -g visual-bell off
      set -g bell-action other
      set-hook -g alert-bell 'run-shell "terminal-notifier -title \"tmux: #{session_name}\" -message \"#{window_name} needs attention\" -sound default -group tmux-#{session_name}-#{window_index}"'


      # Session management via tmux-template (prefix + f) and tmux-tea (prefix + o)
      # Replaces: fzf-sessionizer, M-t, M-1..9, bind S/N

      # UX tweaks
      set -g display-time 2000
      set -g detach-on-destroy off
      set -g set-clipboard on
      set -g pane-border-status top
      set -g pane-border-format " #{pane_index}: #{pane_current_command} [#{b:pane_current_path}] "
    '';
  };

  # Tmuxinator workspaces
  # tmux-which-key configuration
  xdg.configFile."tmux/plugins/tmux-which-key/config.yaml".text = ''
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
    custom_variables:
      - name: log_info
        value: "#[fg=green,italics] [info]#[default]#[italics]"
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
            command: 'confirm-before -p "Kill window #W? (y/n)" kill-window'
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
            command: 'confirm-before -p "Kill pane #P? (y/n)" kill-pane'
          - name: Sync panes
            key: "Y"
            command: setw synchronize-panes
      - name: +Sessions
        key: s
        menu:
          - name: Project picker
            key: f
            command: run-shell ~/.local/bin/tmux-project-picker
          - name: Choose
            key: s
            command: choose-tree -Zs
          - name: New here
            key: n
            command: new-session -c "#{pane_current_path}"
          - name: Tea
            key: t
            command: run "tea"
          - name: Rename
            key: r
            command: command-prompt -I "#S" "rename-session -- \"%%\""
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
            command: "run \"#{@popup-toggle} -w75% -h75% -Ed#{pane_current_path}\""
          - name: Lazygit
            key: g
            command: "run \"#{@popup-toggle} -w90% -h90% -Ed#{pane_current_path} --name=lazygit lazygit\""
          - name: Yazi
            key: "y"
            command: "run \"#{@popup-toggle} -w90% -h90% -Ed#{pane_current_path} --name=yazi yazi\""
          - name: Deploy
            key: d
            command: "run \"#{@popup-toggle} -w80% -h60% -Ed#{pane_current_path} --name=deploy just deploy\""
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
      - code:
          layout: even-horizontal
          panes:
            - nvim .
            - claude --continue
      - ops:
          layout: main-vertical
          panes:
            - bv
            - foreman status --watch
            - ""
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
    root: <%= ENV.fetch('OBSIDIAN_VAULT', File.expand_path('~/Library/Mobile Documents/iCloud~md~obsidian/Documents/ldangelo')) %>
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

  # Multi-agent layout: run several Claude Code sessions in parallel
  xdg.configFile."tmuxinator/agents.yml".text = ''
    name: agents
    root: .
    windows:
      - agent-1:
          panes:
            - claude --continue
      - agent-2:
          panes:
            - claude --continue
      - agent-3:
          panes:
            - claude --continue
      - overview:
          layout: even-horizontal
          panes:
            - br list --status=open
            - ""
  '';

  # Robust project picker for Prefix f. Cancels cleanly and falls back when zoxide is empty.
  home.file.".local/bin/tmux-project-picker" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      set -uo pipefail

      candidates="$(
        {
          zoxide query -l 2>/dev/null || true
          fd --type d --max-depth 3 . "$HOME/Development" "$HOME/code" "$HOME/src" 2>/dev/null || true
        } | awk 'NF' | awk '!seen[$0]++'
      )"

      if [[ -z "$candidates" ]]; then
        tmux display-message "No project dirs found by zoxide/fd"
        exit 0
      fi

      if [[ -n "''${TMUX:-}" ]] && command -v fzf-tmux >/dev/null 2>&1; then
        dir="$(printf '%s\n' "$candidates" | fzf-tmux -p 80%,70% --prompt='project> ')" || exit 0
      else
        dir="$(printf '%s\n' "$candidates" | fzf --prompt='project> ')" || exit 0
      fi

      [[ -n "$dir" ]] || exit 0
      exec "$HOME/.local/bin/tmux-template" "$dir"
    '';
  };

  # Rename a new tmux session to the basename of its starting directory.
  home.file.".local/bin/tmux-auto-rename-session" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      set -euo pipefail

      sid="''${1:?session id required}"
      sleep 0.05

      dir="$(tmux display-message -p -t "$sid:" '#{pane_current_path}' 2>/dev/null || true)"
      [[ -n "$dir" ]] || exit 0

      base="$(basename "$dir" | tr '.:' '__')"
      [[ -n "$base" && "$base" != "/" ]] || exit 0

      current="$(tmux display-message -p -t "$sid" '#{session_name}' 2>/dev/null || true)"
      [[ "$current" == "$base" ]] && exit 0

      name="$base"
      n=2
      while tmux has-session -t "=$name" 2>/dev/null; do
        # Existing session with this name is okay if it is this session.
        existing="$(tmux display-message -p -t "=$name" '#{session_id}' 2>/dev/null || true)"
        [[ "$existing" == "$sid" ]] && exit 0
        name="$base-$n"
        n=$((n + 1))
      done

      tmux rename-session -t "$sid" "$name" 2>/dev/null || true
    '';
  };

  # Directory-aware tmuxp launcher. Detects project type and starts matching layout.
  home.file.".local/bin/tmux-template" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      set -euo pipefail

      dir="''${1:-$PWD}"
      dir="$(cd "$dir" && pwd)"
      session="$(basename "$dir" | tr '.:' '__')"

      if tmux has-session -t "$session" 2>/dev/null; then
        if [[ -n "''${TMUX:-}" ]]; then
          tmux switch-client -t "$session"
        else
          tmux attach-session -t "$session"
        fi
        exit 0
      fi

      template="dev"
      if [[ -f "$dir/.tmux-template" ]]; then
        template="$(tr -d '[:space:]' < "$dir/.tmux-template")"
      elif [[ -f "$dir/package.json" ]]; then
        template="node"
      elif [[ -f "$dir/flake.nix" ]]; then
        template="nix"
      elif [[ -f "$dir/Cargo.toml" ]]; then
        template="rust"
      elif [[ -f "$dir/pyproject.toml" || -f "$dir/requirements.txt" ]]; then
        template="python"
      fi

      tmpdir="$(mktemp -d "''${TMPDIR:-/tmp}/tmux-template.XXXXXX")"
      tmp="$tmpdir/workspace.yaml"
      trap 'rm -rf "$tmpdir"' EXIT

      case "$template" in
        node)
          cat > "$tmp" <<EOF
      session_name: "$session"
      start_directory: "$dir"
      windows:
        - window_name: code
          layout: even-horizontal
          panes:
            - nvim .
            - claude --continue
        - window_name: dev
          layout: even-horizontal
          panes:
            - npm run dev
            - npm test -- --watch
      EOF
          ;;
        nix)
          cat > "$tmp" <<EOF
      session_name: "$session"
      start_directory: "$dir"
      windows:
        - window_name: code
          layout: even-horizontal
          panes:
            - nvim .
            - claude --continue
        - window_name: ops
          layout: main-vertical
          panes:
            - br ready || true
            - nix flake check
            - ""
      EOF
          ;;
        rust)
          cat > "$tmp" <<EOF
      session_name: "$session"
      start_directory: "$dir"
      windows:
        - window_name: code
          layout: even-horizontal
          panes:
            - nvim .
            - claude --continue
        - window_name: cargo
          layout: even-horizontal
          panes:
            - cargo check
            - cargo test
      EOF
          ;;
        python)
          cat > "$tmp" <<EOF
      session_name: "$session"
      start_directory: "$dir"
      windows:
        - window_name: code
          layout: even-horizontal
          panes:
            - nvim .
            - claude --continue
        - window_name: test
          layout: even-horizontal
          panes:
            - uv run pytest || pytest
            - ""
      EOF
          ;;
        dev|*)
          cat > "$tmp" <<EOF
      session_name: "$session"
      start_directory: "$dir"
      windows:
        - window_name: code
          layout: even-horizontal
          panes:
            - nvim .
            - claude --continue
        - window_name: ops
          layout: main-vertical
          panes:
            - bv || br ready || true
            - foreman status --watch || true
            - ""
      EOF
          ;;
      esac

      tmuxp load -y -d "$tmp"
      if [[ -n "''${TMUX:-}" ]]; then
        tmux switch-client -t "$session"
      else
        tmux attach-session -t "$session"
      fi
    '';
  };

  # Ensure which-key's generated init.tmux is writable after each deploy.
  # The plugin regenerates this file on every tmux start; if it's read-only
  # (e.g. from a previous umask or macOS quarantine attribute), the plugin
  # silently fails and prefix+Space reverts to next-layout.
  home.activation.fixWhichKeyPermissions = lib.hm.dag.entryAfter ["linkGeneration"] ''
    mkdir -p "$HOME/.local/share/tmux/plugins/tmux-which-key"
    chmod -f u+w "$HOME/.local/share/tmux/plugins/tmux-which-key/init.tmux" || true
  '';

  # treemux init file — copy from nix store to stable location
  # so the path doesn't change across rebuilds with different store hashes
  home.activation.installTreemuxInit = lib.hm.dag.entryAfter ["writeBoundary"] ''
    _treemux_init_src="${treemux}/share/tmux-plugins/treemux/configs/treemux_init.lua"
    _treemux_init_dst="$HOME/.local/share/tmux/plugins/treemux_init.lua"
    mkdir -p "$HOME/.local/share/tmux/plugins"
    if [[ -f "$_treemux_init_src" ]]; then
      cp -f "$_treemux_init_src" "$_treemux_init_dst"
    fi
  '';
  # tmux-tea: symlink tea.sh from plugin to ~/.local/bin/tea
  home.activation.installTea = lib.hm.dag.entryAfter ["writeBoundary"] ''
    _tea_src="${tmux-tea}/share/tmux-plugins/tmux-tea/bin/tea.sh"
    mkdir -p "$HOME/.local/bin"
    if [[ -f "$_tea_src" ]]; then
      ln -sfnv "$_tea_src" "$HOME/.local/bin/tea"
    fi
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
