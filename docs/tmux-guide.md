**Last Updated**: 2026-08-31
**Architecture**: WezTerm (dumb terminal) + tmux (multiplexer) + sesh (session switching) + worktrunk (worktree management) + tmuxp (ad-hoc layouts)
**Prefix Key**: `Ctrl+Space`
**Configuration**: `modules/home-manager/tmux/default.nix`, `dotfiles/config/wezterm/`

---

## Architecture Overview

```
WezTerm (terminal emulator)
  └── tmux (multiplexer — owns all windows, panes, sessions)
        ├── Session: "foreman" (example layout)
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
              ├── shell      (Prefix t)
              ├── bv         (Prefix b)
              ├── br stats   (Prefix s)
              ├── sesh       (Prefix S)
              ├── worktrunk  (Prefix W)
              └── gh-dash    (Prefix w)
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

# Launch the session picker to pick a project
# Prefix S  (Ctrl+Space, then S)
```

### Daily Workflow

1. Open WezTerm
2. `Prefix S` (sesh picker) to attach an existing session, or `Prefix W` (worktrunk) to switch/create a worktree
3. `Prefix g` for lazygit, `Prefix y` for yazi (popups — no extra tabs)
4. `Prefix d` to detach when done (session keeps running)
5. Tomorrow: open WezTerm, `tmux attach` or `Prefix S` again

---

## Session & Worktree Switching (Prefix S / Prefix W)

**Prefix S** opens **sesh picker**, a fast TUI listing live tmux sessions and zoxide-tracked directories. Use fuzzy search + Enter to attach.

**Prefix W** opens **worktrunk's** `wt switch` picker, an interactive list of git worktrees. Selecting (or creating) one attaches you to it; a post-switch hook (`sesh connect --switch`) automatically brings up a live tmux session on the new worktree path.

| Key | Action |
|-----|--------|
| `Prefix S` | Open sesh picker (fuzzy search + Enter) — sessions + zoxide dirs |
| `Prefix W` | Open worktrunk's `wt switch` picker — create/switch worktrees, auto-attaches tmux |
| `Prefix w` | Open gh-dash — GitHub PR/issue dashboard |

How it works:
1. `Prefix S` — pick a live tmux session or a zoxide-known directory to attach/create
2. `Prefix W` — pick or create a git worktree; the post-switch hook attaches a tmux session automatically
3. Each project/worktree maps to its own tmux session

This means switching between projects, worktrees, and checking PR status are each one keypress away.

---

## Keybindings Reference

All bindings use the prefix `Ctrl+Space` unless marked as **root** (no prefix needed).

### Discovering Keybindings

| Key | Action |
|-----|--------|
| `Prefix h` | Open this help guide in a popup |
| `Prefix ?` | List all keybindings (raw) |


### Sessions

| Key | Action |
|-----|--------|
| `Prefix S` | **sesh** — session/directory picker |
| `Prefix W` | **worktrunk** — worktree picker (`wt switch`) |
| `Prefix p` | **tmux-palette** — command palette (fuzzy search all tools) |
| `Prefix d` | Detach from session |
| `Prefix BTab` | Switch to last session |

### Windows (tabs in the status bar)

| Key | Action |
|-----|--------|
| `Prefix c` | New window (in current path) |
| `Prefix ,` | Rename window |
| `Prefix &` | Close window |
| `Prefix n` | Next window |
| `Prefix P` | Previous window |
| `Prefix 1-9` | Jump to window by number |
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

### Popups (floating)

`Prefix t` uses native tmux `display-popup` directly, matching the old shell popup behavior. Tool popups use native `display-popup` with `if-shell` toggle.

| Key | Action | Size |
|-----|--------|------|
| `Prefix t` | General shell | 75% |
| `Prefix b` | Bead viewer (`bv`) | 75% |
| `Prefix h` | This help guide (glow) | 90% |
| `Prefix g` | **lazygit** — git operations | 90% |
| `Prefix y` | **yazi** — file browser | 90% |
| `Prefix s` | **bead stats** (`br stats`) | 75% |
| `Prefix S` | **sesh** — session picker | 75% |
| `Prefix W` | **worktrunk** — worktree picker (`wt switch`) | 90% |
| `Prefix w` | **gh-dash** — GitHub PR/issue dashboard | 90% |
| `Prefix p` | **tmux-palette** — command palette | 75% |

**Diff view** (not in the table above):

| Key | Action |
|-----|--------|
| `Prefix e` | Toggle diff sidebar (`diffnav --watch`) |

Popups open in the current pane's working directory. `Prefix t` opens a tmux shell popup directly; tool popups use `display-popup -C` to toggle (close/open).

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

Recommended plugin set for this setup:

| Plugin | Why keep/use |
|--------|--------------|
| `vim-tmux-navigator` | Seamless Ctrl+h/j/k/l between tmux + Neovim |
| `resurrect` + `continuum` | Session save/restore |
| `tmux-yank` | Copy mode → macOS clipboard |
| `tmux-toggle-popup` | Lazygit/yazi/shell as persistent popups |
| `tmux-palette` | Command palette (Prefix p) |
| `tmux-fzf` + `fzf-tmux-url` | Fuzzy tmux ops + URL picker |
| `tmux-thumbs` / `extrakto` | Fast copy/extract URLs, paths, hashes |
| `tmux-notify` | macOS alerts when long commands/agents finish |
| `catppuccin`, `cpu`, `battery` | Statusline/theme |

Session/worktree switching is now handled by external tools (sesh, worktrunk) bound to `Prefix S` / `Prefix W` rather than a tmux plugin — see "Session & Worktree Switching" below.

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

### Notifications — AI Needs Input / Process Done

Two paths exist:

1. **Terminal bell → tmux alert hook → macOS notification**
   - Any process can call `bell` or `printf '\a'`.
   - tmux shows session/window-aware notification via `terminal-notifier`.
2. **Pi notify extension → terminal notification + bell**
   - Pi auto-notifies on `agent_end`.
   - Pi `ask_user` tool rings before blocking for input.
   - Agents can call `notify_user` when work is complete, blocked, or waiting for attention.

Manual test:

```bash
bell
# or
printf '\a'
```

### tmux-notify — Process Completion Alerts

Monitors panes and sends a macOS notification when a process finishes (after 5+ seconds). Bell signals from AI tools also trigger notifications — if an agent needs attention, you'll get a macOS alert showing the session and window name.

### Catppuccin Theme

Mocha variant with slanted window status. Status bar shows:
- **Session name** — current tmux session
- **CPU** — live percentage with load icon
- **Battery** — charge with charging/discharging indicator
- **Date/time**

---

## Session Management

### sesh + worktrunk — Session & Worktree Switching

sesh is the tmux session/directory picker; worktrunk is the git worktree lifecycle manager, wired to auto-attach a tmux session on switch via a post-switch hook.

| Command | Action |
|---------|--------|
| `Prefix S` | sesh picker — TUI session/directory picker |
| `Prefix W` | worktrunk (`wt switch`) — interactive worktree picker; auto-attaches a tmux session |
| `Prefix w` | gh-dash — GitHub PR/issue dashboard |

### tmuxp — Ad-hoc Layouts

`mux` loads tmuxp layouts manually when needed for custom setups:

```bash
mux load <layout.yaml>
```

### Suggested Project Layout

A common manual layout for a project session (build these windows/panes yourself, or via a saved sesh/tmuxp layout):

```
Window 1: "code" (even-horizontal)
  ├── nvim .
  └── claude --continue

Window 2: "ops" (main-vertical)
  ├── bv / br ready
  ├── foreman status --watch
  └── shell

Popups (not windows — no tab clutter):
  Prefix g → lazygit
  Prefix y → yazi
  Prefix t → shell
```

Only 2 tabs in the status bar. Lazygit and yazi are a keypress away as popups.

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
Prefix S          # sesh picker — pick your project/session
Prefix 1          # Switch to code window (nvim + claude)
Prefix 2          # Switch to ops window (bv + foreman)
Prefix g          # Toggle lazygit popup
Prefix y          # Toggle yazi popup
```

### Switching Between Projects

```bash
Prefix S          # sesh picker — switch to another session/project
                  # Each project is its own tmux session
Prefix W          # worktrunk — switch to (or create) a git worktree
                  # auto-attaches a tmux session on selection
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

---

## Quick Reference Cheat Sheet

| Key | Action |
|-----|--------|
| `Ctrl+Space` | **Prefix key** (start of every command) |
| `Prefix S` | **sesh** — session/project picker |
| `Prefix W` | **worktrunk** — worktree picker (`wt switch`) |
| `Prefix w` | **gh-dash** — GitHub PR/issue dashboard |
| `Ctrl+h/j/k/l` | Navigate panes (no prefix, works with Neovim) |
| `Prefix g` | Toggle lazygit popup |
| `Prefix y` | Toggle yazi popup |
| `Prefix b` | Toggle bead viewer (`bv`) popup |
| `Prefix s` | Toggle bead stats (`br stats`) popup |
| `Prefix t` | Open shell popup |
| `Prefix z` | Zoom pane (toggle fullscreen) |
| `Prefix c` | New window |
| `Prefix 1-9` | Switch to window N |
| `Prefix \|` | Split horizontal |
| `Prefix -` | Split vertical |
| `Prefix d` | Detach |
| `Prefix h` | This help guide (popup) |
| `Prefix F` | Thumbs (quick copy) |
| `Prefix u` | URL picker |
| `Prefix r` | Reload config |

---

## tmux-palette (Prefix p)

tmux-palette is a command palette that provides fuzzy search across all tools and commands. Open with `Prefix p`.

### Available Categories

| Category | Icon | Description |
|----------|------|-------------|
| Sessions | 🏠 | Sessionizer, session picker, new session |
| Git Tools | 🔧 | Browse git log, show diffs, lazygit |
| Worktrees | 🌳 | Add, list, merge, delete worktrees |

### Git Tools Scripts

Located in `scripts/external/`:

- **`gl`** — Browse git log through fzf with delta for side-by-side diffs
  ```bash
  gl                    # Search commit messages
  gl -S                 # Search code changes
  gl --side             # Show diffs side-by-side
  ```

- **`gd`** — Show git diffs through fzf with delta
  ```bash
  gd                    # Show working tree diffs
  gd --staged           # Show staged changes
  gd --side             # Side-by-side diff
  ```

Both scripts support all `git diff`/`git log` flags and open in browser with `Ctrl+o` (fzf internal binding, no tmux conflict).

### Worktree Integration

Worktree operations are accessible from the palette under the "Worktrees" category:

- **Add new worktree** — Create branch + worktree with `wt switch --create`
- **List worktrees** — Show all worktrees with `wt list`
- **Merge current worktree** — Squash merge with `wt merge`
- **Delete current worktree** — Remove with `wt remove`

Setup:
```bash
git clone https://github.com/eduwass/tmux-palette ~/Development/tmux-palette
cd ~/Development/tmux-palette && bun install
```
