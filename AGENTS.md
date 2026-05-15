# nix-darwin System Configuration

Personal macOS system configuration using Nix flakes, nix-darwin, and home-manager for fully declarative system management.

## Purpose

This repository manages the complete development workstation configuration for macOS (aarch64-darwin):

- **System-level settings** via nix-darwin
- **User environment and dotfiles** via home-manager
- **Package installations** (Nix + Homebrew)
- **Application configurations** (Neovim, Zsh, Git, etc.)
- **Secrets management** via sops-nix
- **Development tools and language runtimes**

## Repository Structure

```
nix/
├── flake.nix                  # Main flake configuration and inputs
├── Justfile                   # Command runner recipes (deploy, update, clean)
├── modules/
│   ├── darwin/               # System-level configuration
│   │   ├── system.nix        # macOS defaults, dock, finder
│   │   ├── homebrew.nix      # Homebrew packages and casks
│   │   ├── services.nix      # launchd services (kanata, karabiner)
│   │   └── sops.nix          # Secrets management
│   ├── home-manager/         # User environment configuration
│   │   ├── packages.nix      # Nix packages
│   │   ├── shell.nix         # Zsh, starship, atuin, direnv
│   │   ├── git.nix           # Git settings
│   │   ├── programs.nix      # fzf, zellij, qutebrowser
│   │   ├── nvim/             # Neovim/LazyVim configuration
│   │   └── emacs/            # Emacs configuration
│   └── flakes/               # Flake input definitions
├── overlays/                 # Custom package overlays (sketchybar-lua)
├── dotfiles/                 # Legacy dotfiles (transitioning to home-manager)
├── secrets/                  # sops-nix encrypted secrets
├── docs/                     # Design documentation (PRD/TRD)
└── Brewfile                  # Legacy Homebrew packages
```

## Key Commands

| Command | Description |
|---------|-------------|
| `just deploy` | Deploy system configuration |
| `just deploy-nc` | Deploy without cache (after flake changes) |
| `just deploy-rebuild` | Force complete rebuild |
| `just debug` | Deploy with verbose output |
| `just up` | Update all flake inputs |
| `just clean` | Remove old generations (>7 days) |
| `just gc` | Garbage collect unused packages |
| `./edit_secrets.sh` | Edit encrypted secrets |

## Agent Instructions

## MANDATORY: Use td for Task Management

Run td usage --new-session at conversation start (or after /clear). This tells you what to work on next.

Sessions are automatic (based on terminal/agent context). Optional:
- td session "name" to label the current session
- td session --new to force a new session in the same context

Use td usage -q after first read.

<!-- br-agent-instructions-v1 -->

---

## Beads Workflow Integration

This project uses [beads_rust](https://github.com/Dicklesworthstone/beads_rust) (`br`/`bd`) for issue tracking. Issues are stored in `.beads/` and tracked in git.

### Essential Commands

```bash
# View ready issues (open, unblocked, not deferred)
br ready              # or: bd ready

# List and search
br list --status=open # All open issues
br show <id>          # Full issue details with dependencies
br search "keyword"   # Full-text search

# Create and update
br create --title="..." --description="..." --type=task --priority=2
br update <id> --status=in_progress
br close <id> --reason="Completed"
br close <id1> <id2>  # Close multiple issues at once

# Sync with git
br sync --flush-only  # Export DB to JSONL
br sync --status      # Check sync status
```

### Workflow Pattern

1. **Start**: Run `br ready` to find actionable work
2. **Claim**: Use `br update <id> --status=in_progress`
3. **Work**: Implement the task
4. **Complete**: Use `br close <id>`
5. **Sync**: Always run `br sync --flush-only` at session end

### Key Concepts

- **Dependencies**: Issues can block other issues. `br ready` shows only open, unblocked work.
- **Priority**: P0=critical, P1=high, P2=medium, P3=low, P4=backlog (use numbers 0-4, not words)
- **Types**: task, bug, feature, epic, chore, docs, question
- **Blocking**: `br dep add <issue> <depends-on>` to add dependencies

### Session Protocol

**Before ending any session, run this checklist:**

```bash
git status              # Check what changed
git add <files>         # Stage code changes
br sync --flush-only    # Export beads changes to JSONL
git commit -m "..."     # Commit everything
git push                # Push to remote
```

### Best Practices

- Check `br ready` at session start to find available work
- Update status as you work (in_progress → closed)
- Create new issues with `br create` when you discover tasks
- Use descriptive titles and set appropriate priority/type
- Always sync before ending session

<!-- end-br-agent-instructions -->
