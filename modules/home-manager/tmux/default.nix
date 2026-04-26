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
    patches = [ ./patches/tmux-tea-workspaces.patch ];
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

  tmuxGuide = ../../../docs/tmux-guide.md;
  tmuxTeaScript = "${tmux-tea}/share/tmux-plugins/tmux-tea/bin/tea.sh";
  tmuxFzfScript = "${tmux-fzf}/share/tmux-plugins/tmux-fzf/main.sh";
  fzfTmuxUrlScript = "${pkgs.tmuxPlugins."fzf-tmux-url"}/share/tmux-plugins/fzf-tmux-url/fzf-url.sh";
  extraktoOpenScript = "${pkgs.tmuxPlugins.extrakto}/share/tmux-plugins/extrakto/scripts/open.sh";
  treemuxToggleScript = "${treemux}/share/tmux-plugins/treemux/scripts/toggle.sh";
  fzfTmuxUrlCommand = "run-shell -b ${fzfTmuxUrlScript}";
  treemuxCommand = "run-shell \\\"${treemuxToggleScript} \\\\\\\"NVIM_APPNAME=nvim-treemux nvim,$HOME/.local/share/tmux/plugins/treemux_init.lua,,/usr/bin/python3,left,40,top,70%,editor,0.5,2,@treemux-refresh-interval-inactive-window,0,focus,nvim-tree\\\\\\\" \\\\\\\"#{pane_id}\\\\\\\"\\\"";
  reloadPopupServerCommand = "run-shell \"tmux -L popup source-file ~/.config/tmux/tmux.conf >/dev/null 2>&1 || true\"";

  tmuxKeys = {
    projectPicker = "f";
    toolbox = "o";
    extract = "e";
    quickCopy = "T";
    popupShell = "t";
    popupGit = "g";
    popupFiles = "y";
    popupDeploy = "D";
    popupHelp = "h";
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
          set -g @thumbs-key ${tmuxKeys.quickCopy}
        '';
      }
      fzf-tmux-url
      {
        plugin = extrakto;
        extraConfig = ''
          set -g @extrakto_key ${tmuxKeys.extract}
        '';
      }
      {
        plugin = tmux-toggle-popup;
        extraConfig = ''
          set -g @popup-autostart on
          bind -N "Open shell popup" ${tmuxKeys.popupShell} run "#{@popup-toggle} -E -d \"#{pane_current_path}\" -w75% -h75% --name=shell"
          bind -N "Open lazygit popup" ${tmuxKeys.popupGit} run "#{@popup-toggle} -E -d \"#{pane_current_path}\" -w90% -h90% --name=lazygit lazygit"
          bind -N "Open Yazi popup" ${tmuxKeys.popupFiles} run "#{@popup-toggle} -E -d \"#{pane_current_path}\" -w90% -h90% --name=yazi yazi"
          bind -N "Run deploy popup" ${tmuxKeys.popupDeploy} run "#{@popup-toggle} -E -d \"#{pane_current_path}\" -w80% -h60% --name=deploy just deploy"
          bind -N "Open tmux guide popup" ${tmuxKeys.popupHelp} run "#{@popup-toggle} -w90% -h90% --name=help glow -p ${tmuxGuide}"
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
      set -g @tmux-fzf-launch-key-assign "${tmuxKeys.toolbox}"
      set -g @tmux-fzf-preview-enabled "true"
      set -g @tmux-fzf-popup-enable "true"
      run-shell "${tmux-fzf}/share/tmux-plugins/tmux-fzf/main.tmux"

      # tmux-tea: fuzzy tmux session manager
      # Note: plugin uses tea.tmux instead of tmux_tea.tmux
      unbind-key -n C-t
      set -g @tea-bind '${tmuxKeys.projectPicker}'
      set -g @tea-alt-bind 'false'
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
      bind -N "Split pane horizontally" | split-window -h -c "#{pane_current_path}"
      bind -N "Split pane vertically" - split-window -v -c "#{pane_current_path}"

      # New window in current path
      bind -N "New window in current directory" c new-window -c "#{pane_current_path}"

      # Vi-style pane resizing
      bind -N "Resize pane left" -r H resize-pane -L 5
      bind -N "Resize pane down" -r J resize-pane -D 5
      bind -N "Resize pane up" -r K resize-pane -U 5
      bind -N "Resize pane right" -r L resize-pane -R 5

      # Vi-style copy mode
      bind -T copy-mode-vi v send-keys -X begin-selection
      bind -T copy-mode-vi y send-keys -X copy-pipe-and-cancel "pbcopy"
      bind -T copy-mode-vi C-v send-keys -X rectangle-toggle
      bind -T copy-mode-vi H send-keys -X start-of-line
      bind -T copy-mode-vi L send-keys -X end-of-line
      bind -N "Enter copy mode" Enter copy-mode
      bind -N "Enter copy mode" v copy-mode

      # Window navigation
#      bind Tab last-window
#      bind BTab switch-client -l

      # Window reordering
      bind -N "Move window left" -r "<" swap-window -d -t -1
      bind -N "Move window right" -r ">" swap-window -d -t +1

      # Mouse toggle
      bind -N "Toggle mouse support" m set -g mouse \; display "Mouse: #{?mouse,ON,OFF}"

      # High-signal plugin bindings with explicit descriptions for `Prefix ?`
      bind -N "Project/session picker" ${tmuxKeys.projectPicker} run-shell ${tmuxTeaScript}
      bind -N "tmux toolbox (sessions, panes, buffers, processes)" ${tmuxKeys.toolbox} run-shell -b ${tmuxFzfScript}
      bind -N "Extract text, paths, or URLs from the current pane" ${tmuxKeys.extract} run-shell "\"${extraktoOpenScript}\" \"#{pane_id}\""
      bind -N "Quick-copy visible text via hints" ${tmuxKeys.quickCopy} thumbs-pick
      bind -N "Reload tmux config" R source-file ~/.config/tmux/tmux.conf \; ${reloadPopupServerCommand} \; display "Config reloaded"

      # Activity monitoring (highlight windows with new output)
      set -g monitor-activity on
      set -g visual-activity off

      # Bell monitoring — Claude Code and other agents send terminal bells
      # when waiting for input. This highlights the window and triggers
      # a macOS notification via terminal-notifier.
      set -g monitor-bell on
      set -g visual-bell off
      set -g bell-action other
      set-hook -g alert-bell 'run-shell "terminal-notifier -title \"tmux: #{session_name}\" -message \"#{window_name} needs attention\" -sound default -group tmux-#{session_name}-#{window_index}"'

      # UX tweaks
      set -g display-time 2000
      set -g detach-on-destroy off
      set -g set-clipboard on
      # Pane borders — heavy lines, bold active border
      set -g pane-border-lines double
      set -g pane-border-style fg=#45475A
      set -g pane-active-border-style fg=#89B4FA,bold

      # Pane border status bar — show pane index in title strip
      set -g pane-border-status top
      set -g pane-border-format "#{?pane_active,#[fg=#89B4FA bold] #P #[default],#[fg=#45475A] #P #[default]}"

      # Background dimming — inactive panes use Catppuccin Crust, active uses Base
      set -g window-style "fg=#BAC2DE,bg=#181825"
      set -g window-active-style "fg=#CDD6F4,bg=#1E1E2E"
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
          - ${reloadPopupServerCommand}
          - display "Config reloaded"
    items:
      - name: Projects
        key: ${tmuxKeys.projectPicker}
        command: "run-shell ${tmuxTeaScript}"
      - name: Toolbox
        key: ${tmuxKeys.toolbox}
        command: "run-shell -b ${tmuxFzfScript}"
      - separator: true
      - name: +Sessions
        key: s
        menu:
          - name: Choose
            key: s
            command: choose-tree -Zs
          - name: Rename
            key: r
            command: command-prompt -I "#S" "rename-session -- \"%%\""
          - name: Detach
            key: d
            command: detach-client
      - name: +Windows
        key: w
        menu:
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
          - name: Move left
            key: "<"
            command: swap-window -d -t -1
          - name: Move right
            key: ">"
            command: swap-window -d -t +1
      - name: +Panes
        key: p
        menu:
          - name: Choose
            key: q
            command: display-panes
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
      - name: +Capture
        key: c
        menu:
          - name: Copy mode
            key: v
            command: copy-mode
          - name: Quick copy
            key: ${tmuxKeys.quickCopy}
            command: thumbs-pick
          - name: Extract text
            key: ${tmuxKeys.extract}
            command: "run-shell \"${extraktoOpenScript}\" \"#{pane_id}\""
          - name: Open URL
            key: u
            command: "${fzfTmuxUrlCommand}"
          - name: Treemux sidebar
            key: "BSpace"
            command: "${treemuxCommand}"
      - separator: true
      - name: +Popups
        key: t
        menu:
          - name: Shell
            key: ${tmuxKeys.popupShell}
            command: "run \"#{@popup-toggle} -E -d \\\"#{pane_current_path}\\\" -w75% -h75% --name=shell\""
          - name: Lazygit
            key: ${tmuxKeys.popupGit}
            command: "run \"#{@popup-toggle} -E -d \\\"#{pane_current_path}\\\" -w90% -h90% --name=lazygit lazygit\""
          - name: Yazi
            key: ${tmuxKeys.popupFiles}
            command: "run \"#{@popup-toggle} -E -d \\\"#{pane_current_path}\\\" -w90% -h90% --name=yazi yazi\""
          - name: Deploy
            key: ${tmuxKeys.popupDeploy}
            command: "run \"#{@popup-toggle} -E -d \\\"#{pane_current_path}\\\" -w80% -h60% --name=deploy just deploy\""
          - name: Help
            key: ${tmuxKeys.popupHelp}
            command: "run \"#{@popup-toggle} -w90% -h90% --name=help glow -p ${tmuxGuide}\""
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

  # Ensure which-key's generated init.tmux is writable after each deploy.
  # The plugin regenerates this file on every tmux start; if it's read-only
  # (e.g. from a previous umask or macOS quarantine attribute), the plugin
  # silently fails and prefix+Space reverts to next-layout.
  home.activation.fixWhichKeyPermissions = lib.hm.dag.entryAfter ["writeBoundary"] ''
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

  # tmuxai configuration — uses OPENAI_API_KEY from .envrc (sops-managed)
  xdg.configFile."tmuxai/config.yaml".text = ''
    models:
      primary:
        provider: openai
        model: gpt-4.1-mini
        api_key: ''${OPENAI_API_KEY}
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
