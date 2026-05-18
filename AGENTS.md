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

## Directory Guides

Each subdirectory has its own AGENTS.md with conventions:

| Directory | Guide |
|-----------|-------|
| `modules/darwin/` | [darwin/AGENTS.md](modules/darwin/AGENTS.md) — system config, Homebrew, services |
| `modules/home-manager/` | [home-manager/AGENTS.md](modules/home-manager/AGENTS.md) — packages, editors, dotfiles |
| `modules/flakes/` | [flakes/AGENTS.md](modules/flakes/AGENTS.md) — flake inputs |
| `overlays/` | [overlays/AGENTS.md](overlays/AGENTS.md) — nixpkgs overrides |
| `secrets/` | [secrets/AGENTS.md](secrets/AGENTS.md) — sops-nix secrets |
| `modules/linux/` | Linux flake (separate from darwin) |
| `docs/` | PRD/TRD design docs |

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
