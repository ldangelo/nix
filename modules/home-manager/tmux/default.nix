{ config, lib, pkgs, ... }:
let
  # hiroppy/tmux-agent-sidebar: tracks Claude Code, Codex, OpenCode, OMP panes across all tmux sessions.
  # Patched fork (ldangelo) adds OMP agent support — built from source.
  tmux-agent-sidebar = let 
    sidebar-src = builtins.path { path = /Users/ldangelo/tmux-agent-sidebar; name = "tmux-agent-sidebar-src"; };
    sidebar-bin = pkgs.rustPlatform.buildRustPackage {
      pname = "tmux-agent-sidebar";
      version = "0.13.0-omp";
      src = sidebar-src;
      cargoLock = {
        lockFile = "${sidebar-src}/Cargo.lock";
      };
      doCheck = false;
    };
  in pkgs.tmuxPlugins.mkTmuxPlugin {
    pluginName = "tmux-agent-sidebar";
    version = "0.13.0-omp";
    rtpFilePath = "tmux-agent-sidebar.tmux";
    src = sidebar-src;
    postInstall = ''
      mkdir -p $out/share/tmux-plugins/tmux-agent-sidebar/bin
      cp ${sidebar-bin}/bin/tmux-agent-sidebar $out/share/tmux-plugins/tmux-agent-sidebar/bin/
      chmod +x $out/share/tmux-plugins/tmux-agent-sidebar/bin/tmux-agent-sidebar
    '';
  };

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

  # tmux-floax: floating scratch pane (omerxx/tmux-floax)
  # Built from source — not in nixpkgs.
  tmux-floax = pkgs.tmuxPlugins.mkTmuxPlugin {
    pluginName = "tmux-floax";
    version = "unstable";
    src = pkgs.fetchFromGitHub {
      owner = "omerxx";
      repo = "tmux-floax";
      rev = "133f526793d90d2caa323c47687dd5544a2c704b";
      sha256 = "sha256-9Hb9dn2qHF6KcIhtogvycX3Z0MoQrLPLCzZXtjGlPHw=";
    };
  };

  # tmux-palette: command palette path (configurable; external Bun project)
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
    prefix = "C-Space";
    mouse = true;
    terminal = "tmux-256color";
    baseIndex = 1;
    escapeTime = 0;
    historyLimit = 50000;
    keyMode = "vi";
    focusEvents = true;
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
          set -g @thumbs-key u
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
      {
        plugin = tmux-toggle-popup;
        extraConfig = ''
          # Real nested tmux session per popup (not raw display-popup) so
          # the prefix table, copy-mode, and same-key dismissal all work
          # inside popups — raw display-popup forwards keys straight to
          # the popup's shell with no prefix lookup at all.
          set -g @popup-toggle-mode 'switch'
        '';
      }
      {
        plugin = tmux-agent-sidebar;
        extraConfig = ''
          # Remap from default prefix+e (diff-sidebar) to prefix+A
          set -g @sidebar_key A
          set -g @sidebar_position left
          set -g @sidebar_width 15%
          # Auto-create sidebar in new windows so OMP panes appear automatically
          set -g @sidebar_auto_create on
        '';
      }
    ];
    extraConfig = ''
      # tmux-fzf: fzf-based session/window/pane/command/keybinding/clipboard/process manager
      # Note: plugin uses main.tmux which reads TMUX_FZF_LAUNCH_KEY env var
      set-environment -g TMUX_FZF_LAUNCH_KEY F
      set -g @tmux-fzf-preview-enabled "true"
      set -g @tmux-fzf-popup-enable "true"
      # tmux-floax: persistent floating scratch pane.
      # Default <prefix>+p conflicts with tmux-palette; default <prefix>+P
      # conflicts with the previous-window binding. Override both and keep the
      # scratch pane conceptually separate from the pop-launch popups under <prefix>+t.
      set -g @floax-bind 'T'
      set -g @floax-bind-menu 'F'
      set -g @floax-session-name 'scratch'
      set -g @floax-width '75%'
      set -g @floax-height '75%'
      run-shell "${tmux-floax}/share/tmux-plugins/tmux-floax/floax.tmux"


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
      # tmux-fzf: fzf-based session/window/pane/command/keybinding/clipboard/process manager
      # Plugin is not in home-manager's plugins list because mkTmuxPlugin defaults to
      # looking for tmux_fzf.tmux, but the plugin provides main.tmux instead. Source it here.
      run-shell "${tmux-fzf}/share/tmux-plugins/tmux-fzf/main.tmux"

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
      # Terminal window title
      set -g set-titles on

      # Activity monitoring
      set -g monitor-activity on
      set -g visual-activity off

      # Bell monitoring — Claude Code / pi agents send terminal BEL when waiting
      # for input. This highlights the window and fires a macOS notification.
      # Use run-shell -b (background/forked) so the hook doesn't block tmux
      # event loop; terminal-notifier -remove prevents duplicate notifications
      # when bells fire in rapid succession.
      set -g monitor-bell on
      set -g bell-action any
      set -g visual-bell off
      set-hook -g alert-bell {
        run-shell -b "terminal-notifier -remove 'tmux-#{session_name}-#{window_index}' >/dev/null 2>&1; terminal-notifier -title 'tmux: #{session_name}' -subtitle 'Window: #{window_name} (#{window_index})' -message 'Pane #{pane_index} (#{pane_current_command}): needs input' -sound default -group 'tmux-#{session_name}-#{window_index}' -execute 'tmux switch-client -t #{session_name}:#{window_index}'"
      }
      # Session management via sesh (prefix + S), worktrunk (prefix + W),
      # and tmux-palette (prefix + p).
      set -g display-time 2000
      set -g detach-on-destroy off
      set -g set-clipboard on
      set -g pane-border-status top
      set -g pane-border-format " #{pane_index}: #{pane_current_command} [#{b:pane_current_path}] "
      # Popup overlays — real nested tmux sessions via tmux-toggle-popup.
      set -g default-command ""
      bind t run "#{@popup-toggle} -w75% -h75% -Ed#{pane_current_path} --name=shell"
      bind h run "#{@popup-toggle} -w90% -h90% --name=help glow -p ${../../../docs/tmux-guide.md}"
      bind b run "#{@popup-toggle} -w75% -h75% -Ed#{pane_current_path} --name=bv $SHELL -lc 'bv; exec $SHELL'"
      bind g run "#{@popup-toggle} -w90% -h90% -Ed#{pane_current_path} --name=lazygit lazygit"
      bind y run "#{@popup-toggle} -w90% -h90% -Ed#{pane_current_path} --name=yazi yazi"
      # gh-dash: full TUI GitHub PR/issue dashboard (replaces the old
      # `workmux list --pr` binding on the same key).
      bind w run "#{@popup-toggle} -w90% -h90% -Ed#{pane_current_path} --name=ghdash gh-dash"
      bind s run "#{@popup-toggle} -w75% -h75% -Ed#{pane_current_path} --name=brstats $SHELL -lc 'br stats; exec $SHELL'"
      # sesh: tmux session manager (lists live tmux sessions + zoxide dirs).
      #
      # This intentionally does NOT go through `#{@popup-toggle}` like the
      # other popup bindings above. tmux-toggle-popup (src/variables.sh:
      # DEFAULT_SOCKET_NAME='popup') always opens popups as a session on a
      # SEPARATE nested tmux server (a second `tmux -L popup ...` client
      # exec'd inside a display-popup overlay — see its src/toggle.sh
      # `open_cmds`), so that the prefix table/copy-mode work inside them.
      # That's the right model for disposable utility popups (lazygit,
      # yazi, bv, br stats), but it's actively wrong for a session
      # *switcher*: `$TMUX` inside such a popup points at the throwaway
      # `popup` socket, not the real one, so `sesh list`/`sesh connect`
      # run there only ever see/target the other popup-name sessions on
      # that nested server — never your real sessions (main, project
      # dirs, etc). `sesh connect --switch` would then call
      # `switch-client` against that nested, invisible popup client
      # instead of the real outer client, so the picked session appeared
      # "trapped" inside the popup and it never closed.
      #
      # A raw `display-popup -E` pane, by contrast, belongs to the same
      # server/client as the pane that invoked it (no second nested tmux
      # client), so `switch-client` run inside it correctly retargets the
      # actual outer client, and `-E` auto-closes the popup as soon as the
      # command exits (i.e. right after the switch already happened).
      # `sesh connect --switch` is the same flag/verb already proven to
      # work from a non-interactive context in the worktrunk `post-switch`
      # hook below (`sesh connect --switch {{ worktree_path }}`).
      # `xargs -I{}` no-ops on empty input, so cancelling the fzf picker
      # (Esc) just closes the popup with no error.
      # NOTE: unlike the `#{@popup-toggle}`-based bindings above, `-d` here
      # must be its own separately-quoted token (`-d '#{...}'`), not the
      # compact `-Ed#{...}` smashed-flag style — tmux's native display-popup
      # argument parser (unlike toggle-popup's bash getopts re-parsing of
      # the same style) fails to even register this bind at all when the
      # format string is smashed directly onto a short flag; confirmed by
      # isolating it with `tmux -f <conf> new-session -d ...; list-keys -a`
      # in a throwaway test server (no visible parse error is printed, it
      # just silently never appears in list-keys).
      bind S display-popup -w75% -h75% -E -d '#{pane_current_path}' -T sesh "$SHELL -lc 'sesh list | fzf --reverse --border | xargs -I{} sesh connect --switch \"{}\"'"
      # worktrunk: git worktree manager. `wt switch` with no branch argument
      # opens its own interactive picker; on selection, the post-switch hook
      # in ~/.config/worktrunk/config.toml runs `sesh connect` on the new
      # worktree path, attaching a live tmux session automatically.
      bind W run "#{@popup-toggle} -w90% -h90% -Ed#{pane_current_path} --name=worktrunk wt switch"
      bind p run-shell "${tmux-palette-path}"
      # tmuxai: AI terminal assistant. Launch in a new window (not a nested
      # popup) so it can observe/drive the real session's panes.
      bind i new-window -c "#{pane_current_path}" tmuxai
      bind P previous-window
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

  # worktrunk user config: wires a post-switch hook so that creating or
  # switching to a worktree (via `bind W` → `wt switch` above) also attaches
  # a live tmux session on the new worktree path, replacing workmux's old
  # add/open flow. `sesh connect --switch` is used (rather than the plain
  # `connect`/attach form) because the hook fires from a background process,
  # not an interactive terminal — see https://worktrunk.dev/hook/.
  #
  # NB: modules/home-manager/ai-worktrees.nix (disabled by default; see
  # flake.nix) previously also wrote xdg.configFile."worktrunk/config.toml"
  # under its own `aiWorktrees.enable` option, which would have collided
  # with this definition. That writer was removed — this is now the single
  # source of truth for worktrunk configuration.
  xdg.configFile."worktrunk/config.toml" = {
    force = true;
    text = ''
      post-switch = "sesh connect --switch {{ worktree_path }}"
    '';
  };

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

  # Runs a command and then drops into an interactive shell, keeping a popup
  # pane open after the command exits (e.g. `tmux-stay-open bv`). Generically
  # useful for ad-hoc popup commands; kept as a standalone utility even though
  # no binding in this file currently depends on it (the `bind b` / `bind s`
  # popups above use the equivalent inline `$SHELL -lc 'cmd; exec $SHELL'`
  # form directly, which works fine in a plain double-quoted tmux command).
  home.file.".local/bin/tmux-stay-open" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      "$@"
      exec "$SHELL"
    '';
  };

  # sudo askpass helper — shows macOS GUI dialog when no TTY is available
  home.file.".local/bin/sudo-askpass" = {
    executable = true;
    text = ''
      #!/bin/bash
      /usr/bin/osascript -e 'display dialog "sudo password:" default answer "" with hidden answer with title "sudo"' -e 'text returned of result' 2>/dev/null
    '';
  };

  # Diff view sidebar — toggle a full-height diffnav pane pinned to the left
  # edge of the current window. Bound to Prefix e in the tmux config above.
  home.file.".local/bin/tmux-diff-sidebar" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      set -uo pipefail

      # If a diff sidebar is already open in this window, close it (toggle off).
      existing="$(tmux show-options -wqv @diff_sidebar 2>/dev/null || true)"
      if [[ -n "$existing" ]] && tmux list-panes -a -F '#{pane_id}' | grep -qx "$existing"; then
        tmux kill-pane -t "$existing"
        tmux set-option -wu @diff_sidebar 2>/dev/null || true
        exit 0
      fi
      # Otherwise open one: a full-height pane pinned to the left edge (-fhb),
      # 40% wide, running diffnav in watch mode so it live-updates.
      path="$(tmux display-message -p '#{pane_current_path}')"
      pane="$(tmux split-window -fhb -l 40% -c "$path" -P -F '#{pane_id}' 'diffnav --watch')"
      tmux set-option -w @diff_sidebar "$pane"
    '';
  };
  home.file.".local/share/tmux/resurrect/.keep" = {
    text = "";
  };
  home.file.".local/share/tmux/continuum/.keep" = {
    text = "";
  };
}
