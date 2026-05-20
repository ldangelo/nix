---
name: help
description: "Show this user guide for Pi coding agent. Use when: user asks how to use pi, pi configuration, available skills, extensions, commands, or needs help getting started."
---

# Pi User Guide

## Getting Started

Install:
```bash
npm install -g --ignore-scripts @earendil-works/pi-coding-agent
```

Authenticate:
```bash
export ANTHROPIC_API_KEY=sk-ant-...
pi
```

## Key Commands (type `/` in editor)

| Command | Description |
|---------|-------------|
| `/login` | OAuth authentication |
| `/model` | Switch models |
| `/settings` | Configure settings |
| `/session` | Show session info |
| `/tree` | Navigate session history |
| `/fork` | Create session from history point |
| `/compact` | Compact (summarize) session history |
| `/export` | Export session to HTML |
| `/reload` | Reload extensions, skills, prompts |
| `/quit` | Exit |

## Keyboard Shortcuts

- **Ctrl+C** — Clear editor (x2 to quit)
- **Escape** — Cancel/abort
- **Ctrl+L** — Model selector
- **Ctrl+P** — Cycle models
- **Shift+Tab** — Cycle thinking level
- **Ctrl+O** — Collapse tool output
- **Ctrl+T** — Collapse thinking blocks
- **Enter** — Queue steering message
- **Alt+Enter** — Queue follow-up message

## Configuration

### Settings
- Global: `~/.pi/agent/settings.json`
- Project: `.pi/settings.json`

### Context Files
Load `AGENTS.md` (or `CLAUDE.md`) from:
- `~/.pi/agent/AGENTS.md` (global)
- Current directory or parents

### System Prompt
Replace with `~/.pi/agent/SYSTEM.md` or append via `APPEND_SYSTEM.md`.

## Skills

Skills are on-demand capabilities loaded automatically or via `/skill:name`.

Installed skills:
- **caveman** — Ultra-compressed communication
- **br** — Beads issue tracker
- **bv** — Beads viewer TUI
- **jj** — Jujutsu (git alternative)
- **exa-cli** — Semantic web search
- **firecrawl-cli** — Web scraping
- **context7-cli** — Library docs
- **sequentialthinking-cli** — Complex reasoning
- **foreman** — Multi-agent orchestration
- **obsidian** — Session logging to vault
- **create-prd** — Product Requirements Docs
- **create-trd** — Technical Requirements Docs
- **implement-trd** — TRD implementation with TDD
- **semantic-web-research** — Broad web research
- **crush-mail** — Email action items to tasks
- **detect-hung-sessions** — Agent recovery

## Extensions

Local extensions in `~/.pi/agent/extensions/`:
- **ask-user** — Interactive user questions
- **auto-commit-on-exit** — Auto-commits on exit
- **bookmark** — Session bookmarks
- **confirm-destructive** — Confirm destructive actions
- **dirty-repo-guard** — Guard on uncommitted repos
- **git-checkpoint** — Git stash per turn
- **handoff** — Context transfer to new session
- **model-status** — Model status bar
- **notify** — Terminal notifications
- **preset** — Named model/tool presets
- **progressive-context** — Smart context disclosure
- **subagent** — Spawn pi subagents
- **todo** — TODO management
- **tokens-per-second** — Speed stats
- **nvim** — Neovim integration

## NPM Packages

Installed via `pi install npm:...`:
- **pi-powerline-footer** — Powerline status bar
- **pi-hooks** — Session lifecycle hooks
- **pi-context** — Context management
- **pi-plan-mode** — Plan mode
- **pi-interactive-shell** — Interactive shell overlay
- **pi-custom-compaction** — Custom compaction rules
- **pi-tools** — Additional tools
- **pi-poly-notify** — Polyfill notifications

## Providers

Built-in: Anthropic, OpenAI, Google, Azure, Groq, xAI, OpenRouter, Vercel, Mistral, and many more.

Custom models via `~/.pi/agent/models.json`.

## Philosophy

- **No MCP** — Build CLI tools or extensions
- **No sub-agents** — Use extensions or spawn instances
- **No permission popups** — Use containers or extensions
- **No plan mode** — Use extensions or files
- **No built-in TODOs** — Use files or extensions

Everything is extensible.
