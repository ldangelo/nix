# Terminal Workspace Guide

**Last Updated**: 2026-03-27
**Architecture**: WezTerm (dumb terminal) + tmux (multiplexer) + tmuxinator (layouts) + sessionizer (project switching)
**Prefix Key**: `Ctrl+Space`
**Configuration**: `modules/home-manager/tmux/default.nix`, `dotfiles/config/wezterm/`

---

## Architecture Overview

```
WezTerm (terminal emulator)
  └── tmux (multiplexer — owns all windows, panes, sessions)
        ├── Session: "foreman" (tmuxinator dev layout)
        │     ├── Window 1: "code"     ← tab in status bar
        │     │     ├── Pane: nvim
        │     │     └── Pane: claude --continue
        │     └── Window 2: "ops"      ← tab in status bar
        │           ├── Pane: bv
        │           ├── Pane: foreman status
        │           └── Pane: shell
        ├── Session: "curantis" (another project)
        │     └── ...
        └── Popups (floating, toggleable)
              ├── lazygit    (Prefix g)
              ├── yazi       (Prefix y)
              └── shell      (Prefix t)
```

**WezTerm** handles: font rendering, colors, clipboard (Cmd+C/V), window chrome.
**tmux** handles: everything else — sessions, windows, panes, copy mode, popups, navigation.

Only tmux windows appear as "tabs" in the status bar. Lazygit and yazi are floating popups that toggle on/off without taking a tab slot.

---

## Getting Started

### First Time

```bash
# Start tmux
tmux

# Launch the sessionizer to pick a project
# Prefix f  (Ctrl+Space, then f)

# Or launch a dev workspace directly
cd ~/Development/Fortium/foreman
tmuxinator start dev
```

### Daily Workflow

1. Open WezTerm
2. `Prefix f` to open the sessionizer — pick your project
3. The `dev` layout starts automatically: code + ops windows
4. `Prefix g` for lazygit, `Prefix y` for yazi (popups — no extra tabs)
5. `Prefix d` to detach when done (session keeps running)
6. Tomorrow: open WezTerm, `tmux attach` or `Prefix f` again

---

## Sessionizer (Prefix f)

The sessionizer is a fuzzy project picker. It creates or attaches to a tmux session per project, using the `dev` tmuxinator layout.

| Key | Action |
|-----|--------|
| `Prefix f` | Open sessionizer (fzf project picker) |

How it works:
1. Queries zoxide for your most-used directories (ranked by frequency)
2. Shows them in fzf for fuzzy selection
3. If a tmux session already exists for that project → switches to it
4. If not → creates a new session using the `dev` tmuxinator layout

This means each project gets its own isolated tmux session with the full dev workspace, and you switch between projects instantly.

---

## Keybindings Reference

All bindings use the prefix `Ctrl+Space` unless marked as **root** (no prefix needed).

### Discovering Keybindings

| Key | Action |
|-----|--------|
| `Prefix Space` | **Which-key menu** — browse all keybindings interactively |
| `Prefix h` | Open this help guide in a popup |
| `Prefix ?` | List all keybindings (raw) |

If you forget a shortcut, press `Prefix Space` — the which-key menu shows everything organized by category.

### Sessions

| Key | Action |
|-----|--------|
| `Prefix f` | **Sessionizer** — fuzzy project picker |
| `Prefix o` | **SessionX** — interactive session picker with preview |
| `Prefix S` | Choose session (built-in picker) |
| `Prefix N` | New session in current directory |
| `Prefix d` | Detach from session |
| `Prefix BTab` | Switch to last session |
| `M-t` | New session in current dir (no prefix) |
| `M-1` to `M-9` | Switch to Nth session by creation order (no prefix) |

### Windows (tabs in the status bar)

| Key | Action |
|-----|--------|
| `Prefix c` | New window (in current path) |
| `Prefix ,` | Rename window |
| `Prefix &` | Close window |
| `Prefix n` | Next window |
| `Prefix p` | Previous window |
| `Prefix 1-9` | Jump to window by number |
| `Prefix w` | Window/session tree picker |
| `Prefix Tab` | Last window (toggle) |
| `Prefix <` | Move window left |
| `Prefix >` | Move window right |

### Panes

| Key | Action |
|-----|--------|
| `Prefix \|` | Split horizontally |
| `Prefix -` | Split vertically |
| `Prefix x` | Close pane |
| `Prefix z` | **Zoom pane** (toggle fullscreen — very useful) |
| `Prefix q` | Show pane numbers (press number to jump) |
| `Prefix {` | Swap pane left |
| `Prefix }` | Swap pane right |
| `Prefix !` | Break pane into its own window |

### Pane Navigation (root — no prefix)

| Key | Action |
|-----|--------|
| `Ctrl+h` | Move left (seamless with Neovim splits) |
| `Ctrl+j` | Move down |
| `Ctrl+k` | Move up |
| `Ctrl+l` | Move right |
| `Ctrl+\` | Last pane |

These work seamlessly across tmux panes and Neovim splits via vim-tmux-navigator. Claude Code is excluded — `Ctrl+h` acts as backspace in Claude Code panes.

### Pane Resizing

| Key | Action |
|-----|--------|
| `Prefix H` | Resize left 5 cells |
| `Prefix J` | Resize down 5 cells |
| `Prefix K` | Resize up 5 cells |
| `Prefix L` | Resize right 5 cells |

### Popups (floating, toggleable)

Popups are floating windows that persist in the background. Press the key once to open, press again to dismiss (the process keeps running). Toggle back anytime to check on it.

| Key | Action | Size |
|-----|--------|------|
| `Prefix g` | **lazygit** — git operations | 90% |
| `Prefix y` | **yazi** — file browser | 90% |
| `Prefix t` | General shell | 75% |
| `Prefix D` | Deploy (`just deploy`) | 80x60% |
| `Prefix h` | This help guide | 90% |

Popups open in the current pane's working directory.

### Copy Mode (vi keys)

| Key | Action |
|-----|--------|
| `Prefix Enter` or `Prefix v` | Enter copy mode |
| `v` | Begin selection |
| `V` | Select line |
| `Ctrl+v` | Rectangle selection |
| `y` | Copy selection (to system clipboard via pbcopy) |
| `q` | Exit copy mode |
| `/` | Search forward |
| `n` / `N` | Next / previous match |
| `H` | Start of line |
| `L` | End of line |

### Utility

| Key | Action |
|-----|--------|
| `Prefix r` | Reload tmux config |
| `Prefix m` | Toggle mouse on/off |

---

## Plugins

### Session Persistence: resurrect + continuum

Sessions auto-save every 10 minutes and restore on tmux start.

| Key | Action |
|-----|--------|
| `Prefix Ctrl+s` | Save session manually |
| `Prefix Ctrl+r` | Restore session manually |

Saved data: window layouts, pane positions, working directories, running commands. Neovim sessions restored via `:mksession`.

### tmux-thumbs — Quick Copy

| Key | Action |
|-----|--------|
| `Prefix F` | Activate hint mode — letters appear next to copyable text (URLs, paths, hashes). Press the letter to copy. |

### tmux-fzf-url — Open URLs

| Key | Action |
|-----|--------|
| `Prefix u` | Fuzzy-find URLs in scrollback, select to open in browser |

### extrakto — Extract Text

| Key | Action |
|-----|--------|
| `Prefix Tab` | Extract words, paths, URLs from pane into fzf picker |

### tmux-notify — Process Completion Alerts

Monitors panes and sends a macOS notification when a process finishes (after 5+ seconds). Bell signals from Claude Code also trigger notifications — if an agent needs attention, you'll get a macOS alert showing the session and window name.

### Catppuccin Theme

Mocha variant with slanted window status. Status bar shows:
- **Session name** — current tmux session
- **CPU** — live percentage with load icon
- **Battery** — charge with charging/discharging indicator
- **Date/time**

---

## Tmuxinator Workspaces

Tmuxinator manages predefined layouts. All workspaces use the current directory as root.

### Available Workspaces

| Workspace | Command | Description |
|-----------|---------|-------------|
| `dev` | `tmuxinator start dev` | **Primary** — code (nvim+claude) + ops (bv+foreman+shell) |
| `claude` | `tmuxinator start claude` | Focused AI pairing: nvim + claude side-by-side |
| `agents` | `tmuxinator start agents` | 3 parallel Claude agents + overview |
| `simple` | `tmuxinator start simple` | Single shell |
| `editor` | `tmuxinator start editor` | Neovim fullscreen |
| `monitor` | `tmuxinator start monitor` | System dashboard: htop, disk, network |
| `notes` | `tmuxinator start notes` | Obsidian vault in nvim |
| `ops` | `tmuxinator start ops` | Shell + logs |

### dev (primary workspace)

```
Window 1: "code" (even-horizontal)
  ├── nvim .
  └── claude --continue

Window 2: "ops" (main-vertical)
  ├── bv (beads viewer)
  ├── foreman status --watch
  └── shell

Popups (not windows — no tab clutter):
  Prefix g → lazygit
  Prefix y → yazi
  Prefix t → shell
```

Only 2 tabs in the status bar. Lazygit and yazi are a keypress away as popups.

### agents (multi-agent workspace)

```
Window 1-3: "agent-1/2/3" — independent claude --continue sessions
Window 4: "overview" — br list + shell (even-horizontal)
```

### notes

Root defaults to Obsidian vault at `~/Library/Mobile Documents/iCloud~md~obsidian/Documents/ldangelo`. Override with `OBSIDIAN_VAULT` env var.

### Managing Workspaces

```bash
tmuxinator list                              # List all workspaces
tmuxinator start dev                         # Start in current directory
tmuxinator start dev --project-root ~/myapp  # Start in specific directory
tmuxinator stop dev                          # Kill the session
```

---

## WezTerm (what it still does)

WezTerm is now a thin terminal emulator. It handles:

| Key | Action |
|-----|--------|
| `Cmd+C` | Copy to clipboard |
| `Cmd+V` | Paste from clipboard |
| `Cmd+Q` | Quit WezTerm |
| `Cmd+T` | New WezTerm tab (rarely needed — use tmux windows instead) |
| `Cmd+1-9` | Switch WezTerm tabs |
| `Cmd+=` / `Cmd+-` | Font size up/down |
| `Cmd+0` | Reset font size |
| `Shift+Enter` | Send escaped Enter (for Claude Code) |
| `Cmd+Shift+D` | Debug overlay |
| `Cmd+Shift+P` | Command palette |

Everything else (splits, panes, sessions, copy mode, search) is handled by tmux.

---

## Common Workflows

### Starting a Coding Session

```bash
# Open WezTerm, then:
Prefix f          # Sessionizer — pick your project
                  # dev layout starts automatically
Prefix 1          # Switch to code window (nvim + claude)
Prefix 2          # Switch to ops window (bv + foreman)
Prefix g          # Toggle lazygit popup
Prefix y          # Toggle yazi popup
```

### Switching Between Projects

```bash
Prefix f          # Sessionizer — pick another project
                  # Each project is its own tmux session
M-1 through M-9   # Quick-switch between sessions (Alt+number)
Prefix o          # SessionX interactive picker with preview
```

### Quick Git Operations

```bash
Prefix g          # lazygit popup opens
                  # Stage, commit, push, etc.
Prefix g          # Press again to dismiss (keeps running)
```

### Detach and Resume

```bash
Prefix d          # Detach — everything keeps running
# Close terminal, go home, come back tomorrow
tmux attach       # Everything is exactly where you left it
```

### Multi-Agent Development

```bash
cd ~/Development/Fortium/foreman
tmuxinator start agents
# Window 1-3: three Claude Code sessions working in parallel
# Window 4: overview with task list
# Each agent works independently, check overview for progress
```

---

## Quick Reference Cheat Sheet

| Key | Action |
|-----|--------|
| `Ctrl+Space` | **Prefix key** (start of every command) |
| `Prefix Space` | **Which-key menu** (discover all shortcuts) |
| `Prefix f` | **Sessionizer** (pick project, auto-layout) |
| `Ctrl+h/j/k/l` | Navigate panes (no prefix, works with Neovim) |
| `Prefix g` | Toggle lazygit popup |
| `Prefix y` | Toggle yazi popup |
| `Prefix t` | Toggle shell popup |
| `Prefix z` | Zoom pane (toggle fullscreen) |
| `Prefix c` | New window |
| `Prefix 1-9` | Switch to window N |
| `Prefix \|` | Split horizontal |
| `Prefix -` | Split vertical |
| `Prefix d` | Detach |
| `Prefix o` | SessionX session picker |
| `Prefix h` | This help guide (popup) |
| `Prefix F` | Thumbs (quick copy) |
| `Prefix u` | URL picker |
| `Prefix r` | Reload config |
| `M-1` to `M-9` | Switch to Nth session (no prefix) |
