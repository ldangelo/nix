# tmux User Guide

**Last updated:** 2026-04-05  
**Prefix:** `Ctrl+Space`  
**Source of truth:** `modules/home-manager/tmux/default.nix`

This setup treats **WezTerm as the terminal** and **tmux as the workspace manager**.
Tmux owns the things you use all day: projects, sessions, windows, panes, popups,
copy mode, saved layouts, and notifications.

The guide below focuses on the **curated keybindings** for this config rather than
trying to restate every builtin tmux binding.

---

## Mental model

- **Session** = one project / one long-lived workspace
- **Window** = a tab inside a session
- **Pane** = a split inside a window
- **Popup** = a floating tool that does not consume a tab slot

Typical flow:

1. Use `Prefix f` to jump into a project.
2. Work mostly in the `dev` tmuxinator layout.
3. Use popups for short-lived tools like lazygit, yazi, or help.
4. Detach with `Prefix d`; resume later exactly where you left off.

---

## Daily workflow

### Start or resume work

```bash
tmux
# then press Prefix f to open the project picker
```

`Prefix f` uses **tmux-tea**:

- searches under `~/Development`
- includes configured tmuxinator workspaces by name, even before they are running
- previews matches at the top
- names sessions from the selected directory basename
- opens the project in the standard tmux workspace flow

If tmux is already running, `tmux attach` is still the quickest way to resume the
last active session.

### Default project layout

The primary workspace is `tmuxinator start dev`:

```text
Window 1: code
  - nvim .
  - claude --continue

Window 2: ops
  - bv
  - foreman status --watch
  - shell
```

Use popups for everything that should stay close at hand without adding tab clutter.

---

## Discovery shortcuts

| Key | Action |
| --- | --- |
| `Prefix Space` | Open the which-key menu |
| `Prefix ?` | Show the described key list |
| `Prefix h` | Open this guide in a popup |
| `Prefix R` | Reload tmux config |

`Prefix ?` is especially useful now that the custom bindings have descriptions.

---

## Curated keybindings

### Projects, sessions, and tmux tools

| Key | Action |
| --- | --- |
| `Prefix f` | Project/session picker (`tmux-tea`) |
| `Prefix o` | tmux toolbox (`tmux-fzf`) for sessions, windows, panes, buffers, processes |
| `Prefix s` | Choose a session |
| `Prefix d` | Detach from the current session |
| `Prefix w` | Choose window / tree view |

### Windows and panes

| Key | Action |
| --- | --- |
| `Prefix c` | New window in the current pane's directory |
| `Prefix n` / `Prefix p` | Next / previous window |
| `Prefix 1-9` | Jump to window by number |
| `Prefix |` | Split horizontally |
| `Prefix -` | Split vertically |
| `Prefix x` | Kill pane |
| `Prefix z` | Zoom/unzoom pane |
| `Prefix q` | Show pane numbers |
| `Prefix <` / `Prefix >` | Move window left / right |
| `Prefix H` / `J` / `K` / `L` | Resize pane left / down / up / right |

### Seamless pane movement (no prefix)

| Key | Action |
| --- | --- |
| `Ctrl+h` / `j` / `k` / `l` | Move between tmux panes and Neovim splits |
| `Ctrl+\` | Jump to the last pane |

This comes from `vim-tmux-navigator`, so pane motion feels the same inside tmux
and Neovim.

### Popups

| Key | Action | Notes |
| --- | --- | --- |
| `Prefix t` | Shell popup | 75% × 75% |
| `Prefix g` | Lazygit popup | opens in the current path |
| `Prefix y` | Yazi popup | opens in the current path |
| `Prefix D` | Deploy popup | runs `just deploy` |
| `Prefix h` | Help popup | renders this guide with `glow` |

Popups are for **context-preserving side tasks**. Use them when you want a tool,
but do not want to create another window.

### Capture, search, and sidebars

| Key | Action |
| --- | --- |
| `Prefix T` | Quick-copy visible text with `tmux-thumbs` hints |
| `Prefix e` | Extract text, paths, or URLs from the current pane with `extrakto` |
| `Prefix u` | URL picker from scrollback |
| `Prefix Backspace` | Toggle the `treemux` file sidebar |
| `Prefix Enter` or `Prefix v` | Enter copy mode |

### Copy mode (vi)

| Key | Action |
| --- | --- |
| `v` | Begin selection |
| `V` | Select line |
| `Ctrl+v` | Toggle rectangle selection |
| `y` | Copy selection to macOS clipboard |
| `/` | Search forward |
| `n` / `N` | Next / previous match |
| `H` / `L` | Start / end of line |
| `q` | Exit copy mode |

### Utility

| Key | Action |
| --- | --- |
| `Prefix m` | Toggle mouse support |
| `Prefix Ctrl+s` | Save tmux session now |
| `Prefix Ctrl+r` | Restore tmux session now |

---

## which-key menu layout

`Prefix Space` opens a curated menu with these groups:

- **Projects** — project picker
- **Toolbox** — tmux-fzf
- **Sessions** — choose / rename / detach
- **Windows** — create, switch, rename, move, kill
- **Panes** — choose, zoom, resize, sync, break out
- **Capture** — copy mode, quick copy, extract, URLs, treemux
- **Popups** — shell, lazygit, yazi, deploy, help

If you forget a key, start here instead of guessing.

---

## Tmuxinator workspaces

| Workspace | Command | Purpose |
| --- | --- | --- |
| `dev` | `tmuxinator start dev` | Primary coding workspace |
| `claude` | `tmuxinator start claude` | Neovim + Claude side-by-side |
| `agents` | `tmuxinator start agents` | Three parallel Claude sessions + overview |
| `simple` | `tmuxinator start simple` | Single shell |
| `editor` | `tmuxinator start editor` | Fullscreen Neovim |
| `monitor` | `tmuxinator start monitor` | htop + disk + network watch |
| `notes` | `tmuxinator start notes` | Obsidian vault workspace |
| `ops` | `tmuxinator start ops` | Shell + logs |

These workspace names are also valid direct `tea` targets now, for example `tea notes`
or `tea ops`.

### Notes workspace

`notes` defaults to:

```text
~/Library/Mobile Documents/iCloud~md~obsidian/Documents/ldangelo
```

Override it with `OBSIDIAN_VAULT` if needed.

---

## Persistence and notifications

### Session persistence

`resurrect` + `continuum` are enabled:

- automatic save every 10 minutes
- restore on tmux start
- pane contents captured
- Neovim restored with session support

### Notifications

`tmux-notify` and the bell hook provide lightweight attention routing:

- long-running commands can notify when they finish
- Claude Code / agent bells highlight the window
- macOS notifications include the session and window name

---

## Recommended habits

1. **Use `Prefix f` for project entry** instead of manually creating sessions.
2. **Keep windows stable** (`code`, `ops`, etc.) and use popups for transient tools.
3. **Prefer `Ctrl+h/j/k/l`** over arrow-key pane hopping.
4. **Use `Prefix o`, `Prefix T`, and `Prefix e`** before copy/pasting manually from scrollback.
5. **Detach instead of closing terminals** so the workspace keeps running.

---

## Quick cheat sheet

| Key | Action |
| --- | --- |
| `Ctrl+Space` | Prefix |
| `Prefix f` | Project/session picker |
| `Prefix o` | tmux toolbox |
| `Prefix g` | Lazygit popup |
| `Prefix y` | Yazi popup |
| `Prefix t` | Shell popup |
| `Prefix T` | Quick-copy with hints |
| `Prefix e` | Extract text from pane |
| `Prefix Backspace` | Toggle treemux sidebar |
| `Prefix z` | Zoom pane |
| `Prefix d` | Detach |
| `Prefix R` | Reload config |
| `Prefix Space` | which-key menu |
| `Ctrl+h/j/k/l` | Navigate panes / Neovim splits |

---

## Improvement ideas to consider later

These are not required to use the current setup, but they are sensible next
steps if you want to keep refining it:

- add a dedicated **scratch popup** or REPL popup
- add a **window rename convention** for `dev`-style sessions
- add **status-bar indicators** for zoomed panes / synchronized panes
- generate a shorter **one-screen cheat sheet** from the same key metadata used by tmux
