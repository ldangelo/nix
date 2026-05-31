---
disclose: summary
summary: nix-darwin workstation repo rules, key deploy commands, caveman response style, beads workflow, and mandatory commit/push session protocol
triggers: [nix, darwin, deploy, home-manager, flakes, overlays, secrets, beads, br, bv, git, commit, push, caveman]
---

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

Respond terse like smart caveman. All technical substance stay. Only fluff die.

Rules:
- Drop: articles (a/an/the), filler (just/really/basically), pleasantries, hedging
- Fragments OK. Short synonyms. Technical terms exact. Code unchanged.
- Pattern: [thing] [action] [reason]. [next step].
- Not: "Sure! I'd be happy to help you with that."
- Yes: "Bug in auth middleware. Fix:"

Switch level: /caveman lite|full|ultra|wenyan
Stop: "stop caveman" or "normal mode"

Auto-Clarity: drop caveman for security warnings, irreversible actions, user confused. Resume after.

Boundaries: code/commits/PRs written normal.

## Beads Issue Tracker

This project uses **br** ([beads_rust](https://github.com/Dicklesworthstone/beads_rust)) for issue tracking, with **bv** (beads viewer) as a graph-aware TUI. Run `br robot-docs` for machine-readable command contracts.

### Quick Reference

```bash
br ready                              # Find available work (open, unblocked)
br show <id>                          # View issue details
br update <id> --status=in_progress   # Claim work
br close <id>                         # Complete work
br sync --flush-only                  # Export DB to .beads/beads.jsonl
bv --robot-triage                     # Graph-aware triage for agents
```

### Rules

- Use `br` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `br robot-docs` for the full command reference
- In agent context use `bv --robot-*` only; bare `bv` launches an interactive TUI that blocks the session

**Architecture in one line:** issues live in a local SQLite DB (`.beads/*.db`); `br sync --flush-only` exports `.beads/beads.jsonl` (committed to git); `br sync` never runs git commands. `bv` reads that JSONL.

## Session Completion

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
