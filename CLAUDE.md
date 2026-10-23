# macOS System Configuration (chezmoi + Homebrew)

Get the task management instructions.

READ AGENTS.md

## Project Overview

Personal macOS workstation setup. Dotfiles are plain files managed by **chezmoi**
(source: `home/`, selected by `.chezmoiroot`), packages and apps by **Homebrew**
(`home/dot_Brewfile.tmpl`), secrets by the **1Password CLI**. Nix (nix-darwin /
home-manager) was removed; do not reintroduce `/nix/store` paths.

**Target**: macOS (aarch64-darwin), user `ldangelo`. Full reference: `docs/CHEZMOI.md`.

## Quick Reference

```bash
just apply          # chezmoi apply (also runs brew bundle, tmux plugins, launchd, ...)
just diff           # preview changes
just re-add <file>  # a tool rewrote its config -> pull it into the source
just add <file>     # start managing a file
just upgrade        # brew upgrade + refresh externals
just check          # fail if nix references sneak back in
```

## Rules for editing the system

- Edit files under `home/` (chezmoi naming: `dot_` = leading ".", `private_`, `executable_`,
  `create_` = write once, `.tmpl` = template), then `just apply`. Or edit the live file and `chezmoi re-add` it.
- There is no build or deploy step. The live files are real, writable files.
- Packages: add to `home/dot_Brewfile.tmpl`. Machine differences use `{{ if .workstation }}`.
- Secrets: never plaintext in the repo. Templates read `op://<vault>/<item>/credential`
  (see `docs/CHEZMOI.md`). Never print secret values.
- Do NOT manage `~/.pi` or `~/.omp` with chezmoi; pi/omp own those directories.
- Work on a branch and keep `main` working; verify with `just diff` before applying.

## Key Locations

- Source root: `/Users/ldangelo/nix/home/` (this repo is cloned at `~/nix`)
- Brewfile: `home/dot_Brewfile.tmpl`; scripts: `home/.chezmoiscripts/`; externals: `home/.chezmoiexternal.toml`
- Shell: `home/dot_zshrc`, `dot_zshenv`, `dot_zprofile`, `home/dot_config/zsh/rc/`
- tmux: `home/dot_config/tmux/tmux.conf` (plugins via TPM, `@plugin` lines)
- Legacy Nix config (kept until cutover is finished): `flake.nix`, `modules/`

## Output Preferences

- Be concise and technical; reference files with line numbers (e.g. `home/dot_zshrc:12`).
- Use `just` recipes over raw chezmoi commands where one exists.
- Explain "why" for non-obvious changes.

---

## Beads Workflow Integration

This project uses [beads_rust](https://github.com/Dicklesworthstone/beads_rust) (`br`/`bd`) for issue tracking. Issues are stored in `.beads/` and tracked in git.

### Essential Commands

```bash
# View ready issues (unblocked, not deferred)
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

- **Dependencies**: Issues can block other issues. `br ready` shows only unblocked work.
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


## Beads Viewer (bv)

`bv` is a graph-aware TUI for browsing the beads tracker (PageRank, critical path, kanban, dependency DAG). It reads `.beads/beads.jsonl`, which `br sync --flush-only` writes.

### Agent usage (robot mode)

Never run bare `bv` in an agent context — it launches an interactive TUI that blocks the session. Always use `--robot-*` flags:

```bash
bv --robot-triage   # Single-call triage: counts, ranked picks, blockers, project health
bv --robot-next     # Minimal: top pick + claim command
bv --robot-help     # Full robot-mode reference
```

Before claiming work surfaced by `bv`, verify state with `br show <id> --json` or `br ready --json`.

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
