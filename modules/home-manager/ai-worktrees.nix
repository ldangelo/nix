# modules/home-manager/ai-worktrees.nix
#
# Parallel AI-agent git-worktree workflow:
#   worktrunk (wt)  — create / run / merge / abandon worktrees
#   clash           — predict merge conflicts across worktrees
#   tmuxinator      — an `agents` layout with a live conflict radar
#
# The agents themselves (OMP / pi) are managed by omp.nix / pi-agent.nix — this
# module deliberately does not touch them.
#
# DISABLED BY DEFAULT so it can't break a deploy. To turn on:
#   1. Set the two `version` fields below to the current crates.io releases
#      (`nix-search-tv worktrunk` / see https://crates.io/crates/clash-sh).
#   2. Set `aiWorktrees.enable = true;` where you set omp.enable (flake.nix, or a module).
#   3. `just deploy` — Nix fails with `got: sha256-...` for each source; paste those
#      into the matching `lib.fakeHash` (src `hash` first, then `cargoHash`).
#   4. `just deploy` again — it builds.

{ pkgs, lib, config, ... }:

let
  cfg = config.aiWorktrees;

  # worktrunk is not in nixpkgs; build from crates.io.
  worktrunk = pkgs.rustPlatform.buildRustPackage rec {
    pname = "worktrunk";
    version = "0.0.0"; # TODO: current https://crates.io/crates/worktrunk
    src = pkgs.fetchCrate {
      inherit pname version;
      hash = lib.fakeHash; # TODO: paste Nix's reported src hash
    };
    cargoHash = lib.fakeHash; # TODO: paste Nix's reported cargoHash
    meta = {
      description = "Git worktree manager for parallel AI agent workflows";
      homepage = "https://github.com/max-sixty/worktrunk";
      mainProgram = "wt";
    };
  };

  # NB: the `clash` in nixpkgs is a *different* tool (proxy). This is clash-sh.
  clash = pkgs.rustPlatform.buildRustPackage rec {
    pname = "clash-sh";
    version = "0.0.0"; # TODO: current https://crates.io/crates/clash-sh
    src = pkgs.fetchCrate {
      inherit pname version;
      hash = lib.fakeHash; # TODO
    };
    cargoHash = lib.fakeHash; # TODO
    meta = {
      description = "Detect merge conflicts across git worktrees for parallel AI agents";
      homepage = "https://github.com/clash-sh/clash";
      mainProgram = "clash";
    };
  };
in
{
  options.aiWorktrees.enable =
    lib.mkEnableOption "worktrunk + clash + tmuxinator parallel-agent worktree workflow";

  config = lib.mkIf cfg.enable {
    home.packages = [ worktrunk clash pkgs.tmuxinator ];

    # NB: this module used to also write xdg.configFile."worktrunk/config.toml"
    # here, but that collided with the config.toml now owned by
    # modules/home-manager/tmux/default.nix (which sets a post-switch hook
    # wiring `wt switch` to `sesh connect`). That block was removed; the
    # tmux module's config.toml is now the single source of truth for
    # worktrunk configuration.

    # ── tmuxinator project:  `tmuxinator start agents`  (run from a repo) ────
    # Large control pane on the left; right column has the clash radar on top
    # and a spare agent pane below. Worktree creation is NOT auto-run — the panes
    # drop you in the repo root with the `wt switch` command echoed, ready to go.
    xdg.configFile."tmuxinator/agents.yml".text = ''
      name: agents
      root: <%= ENV["PWD"] %>
      startup_window: work
      windows:
        - work:
            layout: main-vertical
            panes:
              - control:
                  - clear
                  - echo "wt switch -c feat/a -x omp        # or -x claude / -x codex"
              - radar:
                  - clash watch
              - agent2:
                  - clear
                  - echo "wt switch -c feat/b -x omp"
    '';

    # ── convenience aliases (shell-agnostic; merges with your existing set) ──
    home.shellAliases = {
      ship = "wt merge";   # merge back + cleanup
      nuke = "wt remove";  # abandon
      wts  = "wt list";    # all active agents/worktrees
      cw   = "clash watch"; # conflict radar
    };
  };
}
