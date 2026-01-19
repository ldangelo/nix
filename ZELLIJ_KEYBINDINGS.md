# Zellij Keybindings Reference

## Leader Key System (Ctrl+A)

Zellij uses a **leader key system** similar to tmux/screen. Press `Ctrl+A` followed by a command key to perform operations. This frees up most Ctrl keys for shell and terminal use.

### Core Concepts

- **Leader**: `Ctrl+A` - Entry point for all Zellij operations
- **Seamless Navigation**: `Ctrl+hjkl` - Navigate between Zellij panes AND Neovim splits without entering a mode
- **Essential Shortcuts**: `Ctrl+G` (lock), `Ctrl+S` (scroll), `Ctrl+Q` (quit) - Frequently used, remain as direct Ctrl shortcuts

---

## Quick Reference Card

### Essential Direct Shortcuts (No Leader)

| Keybinding | Action | Description |
|-----------|--------|-------------|
| `Ctrl+h/j/k/l` | Navigate | Seamless navigation (works in Zellij AND Neovim) |
| `Ctrl+G` | Lock mode | Passthrough mode - send all keys to underlying app |
| `Ctrl+S` | Scroll mode | Enter scrollback mode |
| `Ctrl+Q` | Quit | Quit Zellij |

### Leader Key Operations (Ctrl+A + key)

After pressing `Ctrl+A`, press one of these keys:

#### Pane Operations
| Key | Action | Description |
|-----|--------|-------------|
| `n` | New pane | Create horizontal pane |
| `d` | New pane down | Create pane below |
| `r` | New pane right | Create pane to the right |
| `x` | Close pane | Close focused pane |
| `f` | Fullscreen | Toggle pane fullscreen |
| `z` | Zoom | Toggle pane fullscreen (tmux compat) |
| `w` | Float | Toggle floating panes |
| `e` | Embed/Float | Toggle pane embed/floating |
| `p` | Pane mode | Enter pane mode for more operations |
| `h/j/k/l` | Quick focus | Focus left/down/up/right pane |
| `"` | Split down | Create pane below (tmux compat) |
| `%` | Split right | Create pane right (tmux compat) |

#### Tab Operations
| Key | Action | Description |
|-----|--------|-------------|
| `t` | New tab | Create new tab |
| `c` | New tab | Create new tab (tmux compat) |
| `,` | Rename tab | Rename current tab |
| `Tab` | Tab mode | Enter tab mode for more operations |

#### Mode Entries
| Key | Action | Description |
|-----|--------|-------------|
| `R` | Resize mode | Enter resize mode (capital R) |
| `M` | Move mode | Enter move mode (capital M) |
| `S` | Session mode | Enter session mode (capital S) |
| `[` | Scroll mode | Enter scroll mode (tmux compat) |

#### Layout & Session
| Key | Action | Description |
|-----|--------|-------------|
| `Space` | Next layout | Cycle to next swap layout |
| `D` | Detach | Detach from session (capital D) |

#### Special
| Key | Action | Description |
|-----|--------|-------------|
| `Ctrl+A` | Literal Ctrl+A | Sends literal Ctrl+A to terminal |
| `Esc` | Cancel | Exit leader mode |

---

## Detailed Mode Documentation

### Normal Mode (Default)

This is the default mode. Only essential shortcuts are active:
- `Ctrl+hjkl` - Seamless navigation
- `Ctrl+G` - Lock mode
- `Ctrl+S` - Scroll mode  
- `Ctrl+Q` - Quit
- `Ctrl+A` - Enter leader mode

### Locked Mode (Ctrl+G)

Passthrough mode - all keys go to the underlying application.

**Purpose**: Useful when you need to use an application that conflicts with Zellij bindings (e.g., another terminal multiplexer, vim in insert mode with ctrl mappings).

**Exit**: `Ctrl+G`

### Leader Mode (Ctrl+A)

Entry point for all Zellij operations. See "Leader Key Operations" table above.

**Exit**: `Esc` or `Ctrl+A`

### Pane Mode (Ctrl+A p)

Advanced pane management operations.

| Key | Action |
|-----|--------|
| `h/j/k/l` or arrows | Focus pane |
| `n` | New pane (horizontal) |
| `d` | New pane down |
| `r` | New pane right |
| `s` | New pane stacked |
| `x` | Close focused pane |
| `f` | Toggle fullscreen |
| `w` | Toggle floating panes |
| `e` | Toggle pane embed/floating |
| `i` | Toggle pane pinned |
| `z` | Toggle pane frames |
| `c` | Rename pane |
| `p` | Switch focus between panes |
| `Esc` / `Ctrl+A` | Exit to normal mode |

### Tab Mode (Ctrl+A Tab)

Advanced tab management operations.

| Key | Action |
|-----|--------|
| `h/j/k/l` or arrows | Navigate tabs |
| `1-9` | Go to tab 1-9 |
| `n` | New tab |
| `x` | Close tab |
| `r` | Rename tab |
| `s` | Toggle active sync tab |
| `b` / `[` / `]` | Break pane operations |
| `Tab` | Toggle between last two tabs |
| `Esc` / `Ctrl+A` | Exit to normal mode |

### Resize Mode (Ctrl+A R)

Resize panes using hjkl keys.

| Key | Action |
|-----|--------|
| `h/j/k/l` or arrows | Increase size in direction |
| `H/J/K/L` | Decrease size in direction (uppercase) |
| `+` / `=` | Increase size |
| `-` | Decrease size |
| `Esc` / `Ctrl+A` | Exit to normal mode |

**Tip**: Hold keys to continuously resize

### Move Mode (Ctrl+A M)

Move panes to different positions.

| Key | Action |
|-----|--------|
| `h/j/k/l` or arrows | Move pane in direction |
| `n` / `Tab` | Move pane (cycling) |
| `p` | Move pane backwards |
| `Esc` / `Ctrl+A` | Exit to normal mode |

### Scroll Mode (Ctrl+S or Ctrl+A [)

Navigate through scrollback buffer.

| Key | Action |
|-----|--------|
| `h/j/k/l` or arrows | Scroll |
| `u` / `d` | Half-page up/down |
| `Ctrl+B` / `Ctrl+F` | Page up/down |
| `PageUp` / `PageDown` | Page navigation |
| `s` | Enter search mode |
| `e` | Edit scrollback in $EDITOR |
| `Ctrl+C` | Scroll to bottom and exit |
| `Ctrl+S` | Exit to normal mode |

### Search Mode (From scroll mode: s)

Search within scrollback buffer.

| Key | Action |
|-----|--------|
| `n` | Search down (next) |
| `p` | Search up (previous) |
| `c` | Toggle case sensitivity |
| `w` | Toggle whole word |
| `o` | Toggle options |
| `Esc` | Back to scroll mode |
| `Ctrl+C` | Scroll to bottom and exit |

### Session Mode (Ctrl+A S)

Session management and plugins.

| Key | Action |
|-----|--------|
| `w` | Session manager (welcome screen) |
| `a` | About plugin |
| `c` | Configuration plugin |
| `p` | Plugin manager |
| `s` | Share session plugin |
| `d` | Detach from session |
| `Esc` / `Ctrl+A` | Exit to normal mode |

---

## Integration with Other Tools

### Neovim Integration (vim-zellij-navigator)

**Seamless Navigation**: `Ctrl+hjkl` works across both Zellij panes and Neovim splits without any mode switching. The vim-zellij-navigator plugin (v0.3.0) handles this intelligently.

**How it works**:
- When at the edge of a Neovim split, `Ctrl+h/j/k/l` moves to the adjacent Zellij pane
- When in a Zellij pane without Neovim, it navigates between panes normally
- No configuration needed - it just works!

### Shell History (Atuin)

**No conflicts!** 
- Atuin uses `Ctrl+P` for history search
- Zellij no longer uses `Ctrl+P` (moved to leader key system)
- Zellij no longer uses `Ctrl+N` (freed for shell use)

### WezTerm Terminal

WezTerm handles:
- `Cmd+hjkl` - WezTerm pane resizing
- `Cmd+Space` - WezTerm leader key
- `Cmd+t` - New WezTerm tab

Zellij handles:
- `Ctrl+A` - Zellij leader key
- `Ctrl+hjkl` - Seamless navigation
- Pane/tab operations through leader key

**No conflicts** - Clean separation of concerns.

### AeroSpace Window Manager

AeroSpace uses Alt key for all window management, leaving Ctrl completely free for Zellij and terminal operations.

**No conflicts** - Perfect coexistence.

---

## Conflict Resolution History

### Why Leader Key (Ctrl+A)?

**Problems with modal Ctrl keys** (old system):
- `Ctrl+N` - Conflicted with zsh history navigation
- `Ctrl+P` - Conflicted with Atuin shell history
- `Ctrl+T` - Conflicted with terminal expectations
- `Ctrl+Space` - Generic, not memorable
- `Ctrl+O` - Vim jumplist conflict
- `Ctrl+B` - Scroll conflict

**Solution**: Leader key system
- **Only ONE Ctrl key used**: `Ctrl+A`
- All other Ctrl keys free for shell, vim, terminal
- Mnemonic second keys (n=new, r=resize, etc.)
- Familiar to tmux/screen users

### Why Ctrl+A (not Ctrl+B)?

- `Ctrl+B` is tmux default (we have tmux compat mode anyway)
- `Ctrl+A` is screen default and more ergonomic (left pinky)
- `Ctrl+A` + `Ctrl+A` sends literal Ctrl+A if needed
- More natural for left-handed leader key

---

## Tips & Tricks

### Quick Pane Creation
`Ctrl+A d` - New pane below (mnemonic: **d**own)
`Ctrl+A r` - New pane right (mnemonic: **r**ight)

### Fullscreen Toggle
`Ctrl+A f` - Quickly maximize a pane, then exit back (mnemonic: **f**ullscreen)

### Tab Navigation
`Ctrl+A 1-9` - Jump directly to tab 1-9

### Emergency Exit
`Ctrl+Q` - Quick quit without confirmation

### Literal Ctrl+A
`Ctrl+A Ctrl+A` - Sends literal Ctrl+A to shell (for bash/zsh beginning-of-line)

### Tmux Users
Most tmux bindings work! Just use `Ctrl+A` instead of `Ctrl+B`:
- `Ctrl+A "` - Split down (like tmux)
- `Ctrl+A %` - Split right (like tmux)
- `Ctrl+A c` - New tab (like tmux)
- `Ctrl+A ,` - Rename tab (like tmux)
- `Ctrl+A [` - Scroll mode (like tmux)

---

## Default Layout

The default layout launches:
- **Left pane**: Neovim (`nvim .`)
- **Right pane**: Claude CLI (`claude --continue`)
- **Split**: Horizontal

---

## Configuration Files

- **Main config**: `/Users/ldangelo/nix/modules/home-manager/zellij/config.kdl`
- **Default layout**: `/Users/ldangelo/nix/modules/home-manager/zellij/layouts/default.kdl`
- **This documentation**: `/Users/ldangelo/nix/ZELLIJ_KEYBINDINGS.md`

---

## Theme

**Catppuccin Macchiato** - Matches your Neovim and WezTerm theme for visual consistency.

---

## Plugin System

Loaded plugins:
- `vim-zellij-navigator` (v0.3.0) - Seamless Neovim/Zellij navigation
- `about` - About dialog
- `compact-bar` - Minimal status bar
- `configuration` - Runtime configuration
- `plugin-manager` - Plugin management
- `session-manager` - Session persistence
- `status-bar` - Status information
- `strider` - File picker
- `tab-bar` - Tab display
- `welcome-screen` - Welcome/session manager

---

## Summary

**Philosophy**: 
- **One leader key** (`Ctrl+A`) for all operations
- **Seamless navigation** (`Ctrl+hjkl`) works everywhere
- **Essential shortcuts** remain direct (`Ctrl+G`, `Ctrl+S`, `Ctrl+Q`)
- **Zero conflicts** with shell, vim, terminal, window manager
- **Mnemonic keys** after leader (intuitive, easy to remember)

**Result**: Clean, conflict-free, ergonomic terminal multiplexer that plays nicely with your entire workflow.
