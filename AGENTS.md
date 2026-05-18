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
│   ├── darwin/               # System-level configuration (see modules/darwin/AGENTS.md)
│   │   ├── system.nix        # macOS defaults, dock, finder
│   │   ├── homebrew.nix      # Homebrew packages and casks
│   │   ├── services.nix      # launchd services (kanata, karabiner)
│   │   └── sops.nix          # Secrets management
│   ├── home-manager/         # User environment (see modules/home-manager/AGENTS.md)
│   │   ├── packages.nix      # Nix packages
│   │   ├── shell.nix         # Zsh, starship, atuin, direnv
│   │   ├── git.nix           # Git settings
│   │   ├── programs.nix      # fzf, zellij, qutebrowser
│   │   ├── nvim/             # Neovim/LazyVim configuration
│   │   ├── emacs/            # Emacs configuration
│   │   └── tmux/             # Tmux configuration
│   └── flakes/               # Flake input definitions (see modules/flakes/AGENTS.md)
├── overlays/                 # Custom package overlays (see overlays/AGENTS.md)
├── dotfiles/                 # Legacy dotfiles (transitioning to home-manager)
├── secrets/                  # sops-nix encrypted secrets (see secrets/AGENTS.md)
└── docs/                     # Design documentation (PRD/TRD)
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

## Session Protocol

**Before ending any session, run this checklist:**

```bash
git status              # Check what changed
git add <files>         # Stage code changes
br sync --flush-only    # Export beads changes to JSONL
git commit -m "..."     # Commit everything
git push                # Push to remote
```

<!-- br-agent-instructions-v1 -->
