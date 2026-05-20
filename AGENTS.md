---
disclose: summary
summary: nix-darwin workstation repo rules, key deploy commands, caveman response style, beads workflow, and mandatory commit/push session protocol
triggers: [nix, darwin, deploy, home-manager, flakes, overlays, secrets, beads, bd, git, commit, push, caveman]
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
bd sync --flush-only    # Export beads changes to JSONL
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

<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:7510c1e2 -->
## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

**Architecture in one line:** issues live in a local Dolt DB; sync uses `refs/dolt/data` on your git remote; `.beads/issues.jsonl` is a passive export. See https://github.com/gastownhall/beads/blob/main/docs/SYNC_CONCEPTS.md for details and anti-patterns.

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
<!-- END BEADS INTEGRATION -->
