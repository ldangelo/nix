---
disclose: summary
summary: chezmoi + Homebrew workstation repo rules, key commands, caveman response style, beads workflow, and mandatory commit/push session protocol
triggers: [chezmoi, brew, homebrew, dotfiles, secrets, 1password, beads, br, bv, git, commit, push, caveman]
---

# macOS System Configuration (chezmoi + Homebrew)

Personal macOS workstation. Dotfiles = plain files via chezmoi (`home/`), packages = Homebrew
(`home/dot_Brewfile.tmpl`), secrets = 1Password CLI. Nix was removed. See `docs/CHEZMOI.md`.

## Key Commands

| Command | Description |
| --------- | ------------- |
| `just apply` | Apply dotfiles + run chezmoi scripts (brew bundle, tmux plugins, launchd) |
| `just diff` | Preview what apply would change |
| `just re-add <file>` | Pull a live edit of a managed file back into `home/` |
| `just add <file>` | Start managing a file |
| `just brew` | Install everything in the Brewfile (no upgrades) |
| `just upgrade` | brew upgrade + refresh externals |
| `just check` | Fail if nix references reappear |

Edit under `home/` (or the live file + `chezmoi re-add`). No build/deploy step. Never touch `~/.pi` / `~/.omp`.

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

- Update documentation and user guides when changes are made
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
