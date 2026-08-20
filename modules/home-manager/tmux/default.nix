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

  # tmux-palette: command palette path (configurable)
  tmux-palette-path = "/Users/ldangelo/Development/tmux-palette/bin/tmux-palette.sh";
in
{
   programs.tmux = {
    enable = true;
    # Use Homebrew tmux instead of the Nix-store one. The Nix tmux 3.7b has
    # been SIGSEGV-crashing on fork pre-exec on macOS 26.5.2. Switch the build
    # to verify the underlying cause is the Nix-store binary, not the binary
    # itself.
    package = pkgs.runCommand "homebrew-tmux" { } ''
      mkdir -p $out/bin
      ln -s /opt/homebrew/bin/tmux $out/bin/tmux
    '';
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

      # tmux-palette: command palette (replaces tmux-tea + tmux-which-key)
      run-shell "${tmux-palette-path}"

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

      # Required for wezterm ↔ tmux integration: lets wezterm pass through
      # OSC sequences, images, and user vars from tmux panes. Without this,
      # the wezterm GUI can't read tmux state for native rendering.
      set -g allow-passthrough on

      # Renumber windows when one is closed
      set -g renumber-windows on

      # Split panes with | and -
      bind | split-window -h -c "#{pane_current_path}"
      bind - split-window -v -c "#{pane_current_path}"

      # Diff view — toggle a full-height diffnav pane pinned to the left edge
      bind e run-shell '~/.local/bin/tmux-diff-sidebar'

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

      # Session management via tmux-template (prefix + f) and tmux-palette (prefix + o)
      # Replaces: fzf-sessionizer, M-t, M-1..9, bind S/N

      # UX tweaks
      set -g display-time 2000
      set -g detach-on-destroy off
      set -g set-clipboard on
      set -g pane-border-status top
      set -g pane-border-format " #{pane_index}: #{pane_current_command} [#{b:pane_current_path}] "
      # Popup overlays — native display-popup with if-shell toggle.
      # "display-popup -C" closes the topmost popup; the false branch opens the requested one.
      bind t if-shell "display-popup -C" "" "display-popup -w75% -h75% -E -d '#{pane_current_path}'"
      bind h if-shell "display-popup -C" "" "display-popup -w90% -h90% glow -p ${../../../docs/tmux-guide.md}"
      bind b if-shell "display-popup -C" "" "display-popup -w75% -h75% -E -d '#{pane_current_path}' 'bv'"
      bind g if-shell "display-popup -C" "" "display-popup -w90% -h90% -E -d '#{pane_current_path}' lazygit"
      bind y if-shell "display-popup -C" "" "display-popup -w90% -h90% -E -d '#{pane_current_path}' yazi"
      bind s if-shell "display-popup -C" "" "display-popup -w75% -h75% -E -d '#{pane_current_path}' 'br stats'"

      # tmux-palette: command palette
      bind o run-shell "${tmux-palette-path}"
      # Pin resurrect/continuum save dir inside the home-manager-managed tree
      # so the default ~/.tmux/ path doesn't silently fail and the `-N` clone
      # sessions stop appearing on every server restart. Directories are
      # pre-created by home.file below so the save script never has to.
      set -g @resurrect-dir "$HOME/.local/share/tmux/resurrect"
      set -g @continuum-save-dir "$HOME/.local/share/tmux/continuum"
      set -g @continuum-save-last-timestamp-on-start 'on'
    '';
  };

  # Force-overwrite the auto-generated tmux.conf when its source hash changes.
  # programs.tmux writes here via xdg.configFile; without force=true, home-manager
  # refuses to clobber an existing symlink whose target hash drifted.
  xdg.configFile."tmux/tmux.conf".force = true;

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
    root: ~/code
    windows:
      - editor:
          layout: main-vertical
          panes:
            - nvim .
            - ls -la
  '';

  xdg.configFile."tmuxinator/default.yml".text = ''
    name: default
    root: ~/code
    windows:
      - editor:
          layout: main-vertical
          panes:
            - nvim .
            - ls -la
      - logs:
          layout: horizontal
          panes:
            - tail -f /var/log/syslog
            - tail -f /var/log/auth.log
  '';

}
