# tmux User Guide

**Last Updated**: 2026-02-17
**Prefix Key**: `Ctrl+Space`
**Configuration**: `modules/home-manager/tmux/default.nix`

---

## Core Concepts

tmux is a terminal multiplexer — it lets you run multiple terminal sessions inside a single window, detach from them, and reattach later.

```
Session (named group of windows)
  └── Window (like a browser tab)
        └── Pane (split within a window)
```

- **Sessions** persist even when you close your terminal
- **Windows** are full-screen views within a session
- **Panes** are splits within a window

---

## Getting Started

### The Prefix Key

Almost every tmux command starts with the **prefix key**: `Ctrl+Space`.

Press `Ctrl+Space`, release, then press the action key. For example:
- `Ctrl+Space` then `c` → new window
- `Ctrl+Space` then `|` → split horizontally

### Starting tmux

```bash
# Start a new session
tmux new -s work

# Attach to existing session
tmux attach -t work

# List sessions
tmux ls

# Use a tmuxinator workspace (recommended)
tmuxinator start dev
```

---

## Keybindings Reference

All bindings use the prefix `Ctrl+Space` unless marked as **root** (no prefix needed).

### Sessions

| Key | Action |
|-----|--------|
| `Prefix d` | Detach from session |
| `Prefix S` | Choose session (interactive picker) |
| `Prefix $` | Rename session |
| `Prefix (` | Previous session |
| `Prefix )` | Next session |

### Windows

| Key | Action |
|-----|--------|
| `Prefix c` | New window (in current path) |
| `Prefix ,` | Rename window |
| `Prefix &` | Close window |
| `Prefix n` | Next window |
| `Prefix p` | Previous window |
| `Prefix 1-9` | Jump to window by number |
| `Prefix w` | Window picker |

### Panes

| Key | Action |
|-----|--------|
| `Prefix \|` | Split horizontally |
| `Prefix -` | Split vertically |
| `Prefix x` | Close pane |
| `Prefix z` | Toggle pane zoom (fullscreen) |
| `Prefix q` | Show pane numbers |
| `Prefix {` | Swap pane left |
| `Prefix }` | Swap pane right |
| `Prefix Space` | Cycle layouts |

### Pane Navigation (root — no prefix)

| Key | Action |
|-----|--------|
| `Ctrl+h` | Move left (works across Neovim splits) |
| `Ctrl+j` | Move down |
| `Ctrl+k` | Move up |
| `Ctrl+l` | Move right |
| `Ctrl+\` | Last pane |

### Pane Resizing

| Key | Action |
|-----|--------|
| `Prefix H` | Resize left 5 cells |
| `Prefix J` | Resize down 5 cells |
| `Prefix K` | Resize up 5 cells |
| `Prefix L` | Resize right 5 cells |

### Copy Mode (vi keys)

| Key | Action |
|-----|--------|
| `Prefix [` | Enter copy mode |
| `v` | Begin selection |
| `y` | Copy selection (to system clipboard) |
| `q` | Exit copy mode |
| `/` | Search forward |
| `?` | Search backward |
| `n` / `N` | Next / previous match |

### Utility

| Key | Action |
|-----|--------|
| `Prefix r` | Reload config |
| `Prefix ?` | Command palette |
| `Prefix Backspace` | Command palette (root) |

---

## Plugins

### Session Persistence: resurrect + continuum

Sessions are automatically saved every 10 minutes and restored on tmux start.

| Key | Action |
|-----|--------|
| `Prefix Ctrl+s` | Save session manually |
| `Prefix Ctrl+r` | Restore session manually |

Saved data includes: window layouts, pane positions, working directories, and running commands. Neovim sessions are restored via `:mksession`.

### tmux-thumbs — Quick Copy

Press `Prefix Space` to activate hint mode. Letters appear next to copyable text (URLs, paths, hashes, IPs). Press the letter to copy that text to the clipboard.

| Key | Action |
|-----|--------|
| `Prefix Space` | Activate thumbs hint mode |

### tmux-fzf-url — Open URLs

Scans the visible scrollback for URLs and presents them in an fzf picker. Select one to open in your browser.

| Key | Action |
|-----|--------|
| `Prefix u` | Fuzzy-find URLs in scrollback |

### extrakto — Extract Text

Opens an fzf picker with all words, paths, and URLs from the current pane. Select to insert at cursor or copy to clipboard.

| Key | Action |
|-----|--------|
| `Prefix Tab` | Extract text from pane |

### tmux-yank — System Clipboard

Automatically integrates copy mode with macOS `pbcopy`. Selections made with `y` in copy mode go straight to the system clipboard.

### tmux-toggle-popup — Floating Popups

Toggleable floating popup windows that persist in the background. Each named popup gets its own tmux session — dismiss it and it keeps running, toggle it back to check on it.

| Key | Action |
|-----|--------|
| `Prefix t` | Toggle a general shell popup (75% size) |
| `Prefix g` | Toggle lazygit popup (90% size) |
| `Prefix D` | Toggle deploy popup (`just deploy`, 80x60%) |

Popups open in the current pane's working directory. Press the same key again to dismiss (the process keeps running). Great for:
- `Prefix D` → kick off a deploy, dismiss, keep coding, toggle back to check
- `Prefix g` → quick git operations without leaving your workflow
- `Prefix t` → ad-hoc shell for anything

### Catppuccin Theme

Mocha variant with rounded window status. Status bar shows session name and date/time on the right.

---

## Tmuxinator Workspaces

Tmuxinator manages predefined tmux layouts. All workspaces use the current directory as root by default.

### Available Workspaces

| Workspace | Command | Description |
|-----------|---------|-------------|
| `simple` | `tmuxinator start simple` | Single shell window |
| `editor` | `tmuxinator start editor` | Neovim fullscreen |
| `dev` | `tmuxinator start dev` | Full dev environment: nvim+claude, tasks, git, files |
| `monitor` | `tmuxinator start monitor` | 4-pane dashboard: htop, disk, network, shell |
| `claude` | `tmuxinator start claude` | Side-by-side: nvim + claude --continue |

### Workspace Details

**simple** — Minimal session with a single shell. Good starting point when you just need a terminal.

**editor** — Opens Neovim in the current directory. Use when you just want to edit without extra tooling.

**dev** — Full development workspace with 4 windows:
1. **claude** — nvim + claude side-by-side (even-horizontal)
2. **tasks** — td task monitor
3. **git** — lazyjj for version control
4. **files** — yazi file manager

**monitor** — System monitoring dashboard (tiled layout):
- htop (processes)
- disk usage (df -h, refreshes every 2s)
- network connections (netstat, refreshes every 2s)
- empty shell for ad-hoc commands

**claude** — Focused AI pairing session with nvim and claude --continue side-by-side.

### Managing Workspaces

```bash
# List all workspaces
tmuxinator list

# Start a workspace
tmuxinator start dev

# Start in a specific directory
tmuxinator start dev --project-root ~/projects/myapp

# Stop a workspace (kills the session)
tmuxinator stop dev
```

---

## Common Workflows

### Starting a Coding Session

```bash
# Navigate to your project
cd ~/projects/myapp

# Start the dev workspace
tmuxinator start dev

# You get: nvim+claude, task monitor, lazyjj, yazi
# Switch windows with Prefix+1-4
```

### Claude Pairing

```bash
cd ~/projects/myapp
tmuxinator start claude

# Left pane: nvim for editing
# Right pane: claude for AI assistance
# Ctrl+h/l to move between panes
```

### System Monitoring

```bash
tmuxinator start monitor

# 4-pane tiled view of system health
# Bottom-right pane is free for running commands
```

### Session Persistence

Sessions auto-save every 10 minutes (via continuum). If tmux crashes or you reboot:

```bash
# Just start tmux — continuum auto-restores
tmux

# Or manually restore
# Prefix Ctrl+r
```

### Detach and Reattach

```bash
# Detach: Prefix d (or close the terminal — session survives)

# Later, reattach:
tmux ls            # see what's running
tmux attach -t dev # reconnect
```

---

## Quick Reference Cheat Sheet

| Key | Action |
|-----|--------|
| `Ctrl+Space` | **Prefix key** (start of every command) |
| `Ctrl+h/j/k/l` | Navigate panes (no prefix, works with Neovim) |
| `Prefix c` | New window |
| `Prefix 1-9` | Switch to window N |
| `Prefix \|` | Split horizontal |
| `Prefix -` | Split vertical |
| `Prefix z` | Zoom pane (toggle fullscreen) |
| `Prefix d` | Detach |
| `Prefix S` | Session picker |
| `Prefix [` | Copy mode (vi keys) |
| `Prefix Ctrl+s` | Save session |
| `Prefix Ctrl+r` | Restore session |
| `Prefix Space` | Thumbs (quick copy) |
| `Prefix u` | URL picker |
| `Prefix Tab` | Extract text |
| `Prefix ?` | Command palette |
| `Prefix r` | Reload config |
| `Prefix t` | Popup shell |
| `Prefix g` | Popup lazygit |
| `Prefix D` | Popup deploy (`just deploy`) |
