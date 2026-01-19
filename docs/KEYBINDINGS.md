# Unified Keyboard Shortcut Strategy

**Last Updated**: 2026-01-19  
**Status**: Conflict-Free ✅

This document provides a complete reference for keyboard shortcuts across all tools in your development environment. The strategy is designed to eliminate conflicts, maximize muscle memory, and maintain ergonomic efficiency.

---

## Architecture Overview

Your keyboard strategy follows a **five-layer hierarchy** with clear modifier separation:

```
Layer 1: OS/Window Manager (AeroSpace)     → Alt-based
Layer 2: Terminal Emulator (WezTerm)       → Cmd-based + Cmd+Space leader
Layer 3: Terminal Multiplexer (Zellij)     → Ctrl+A leader + Ctrl+hjkl seamless
Layer 4: Shell (zsh)                       → Vi-mode, Atuin (Ctrl+P)
Layer 5: Editor (Neovim)                   → Space leader, Cmd for resizing
```

### Modifier Key Allocation

| Modifier | Primary Tool | Usage Pattern | Rationale |
|----------|--------------|---------------|-----------|
| **Alt** | AeroSpace | Window/workspace management | OS-level operations, intercepted first |
| **Cmd** | WezTerm, Neovim | Terminal tabs, pane resizing | macOS standard, terminal-scoped |
| **Ctrl** | Zellij, Neovim | Navigation, leader access | Cross-tool consistency |
| **Space** | Neovim, WezTerm | Leader key prefix | Ergonomic, easy to reach |

---

## Core Navigation Principles

### The Crown Jewel: Ctrl+hjkl Seamless Navigation

**Everywhere navigation works the same:**
```
Ctrl+H → Move focus LEFT  (works in WezTerm, Zellij, AND Neovim)
Ctrl+J → Move focus DOWN
Ctrl+K → Move focus UP
Ctrl+L → Move focus RIGHT
```

**How it works:**
1. **WezTerm** detects if you're in Neovim via `IS_NVIM` user var
2. **Zellij** uses vim-zellij-navigator plugin for Neovim detection
3. **Neovim** smart-splits handles seamless movement to edges
4. Result: **Zero context switching** - just navigate!

### Resizing Consistency: Cmd+hjkl

```
Cmd+H → Resize LEFT   (works in WezTerm panes AND Neovim splits)
Cmd+J → Resize DOWN
Cmd+K → Resize UP
Cmd+L → Resize RIGHT
```

**Avoids**: Conflict with AeroSpace's Alt+hjkl (window focus)

---

## Layer 1: AeroSpace (Window Manager)

### Window Focus Navigation
| Keybinding | Action |
|-----------|--------|
| `Alt+H` / `Alt+Left` | Focus window left |
| `Alt+J` | Focus window down |
| `Alt+K` | Focus window up |
| `Alt+L` / `Alt+Right` | Focus window right |
| `Alt+F` | Toggle fullscreen |

### Window Movement
| Keybinding | Action |
|-----------|--------|
| `Alt+Shift+H` | Move window left |
| `Alt+Shift+J` | Move window down |
| `Alt+Shift+K` | Move window up |
| `Alt+Shift+L` | Move window right |

### Window Resizing
| Keybinding | Action |
|-----------|--------|
| `Alt+Shift+-` | Decrease size (-50px) |
| `Alt+Shift+=` | Increase size (+50px) |

### Workspace Navigation
| Keybinding | Workspace |
|-----------|-----------|
| `Alt+1` | Home (Terminal/WezTerm) |
| `Alt+D` | Development (IntelliJ) |
| `Alt+C` | Communication (Slack, Zoom, Messages) |
| `Alt+O` | Organization (Fantastical) |
| `Alt+T` | Trading (TradingView) |
| `Alt+Tab` | Toggle last two workspaces |

### Move Window to Workspace
| Keybinding | Action |
|-----------|--------|
| `Alt+Shift+1-9` | Move to workspace 1-9 |
| `Alt+Shift+A-Z` | Move to workspace A-Z |
| `Alt+Shift+Tab` | Move workspace to next monitor |

### Service Mode (Advanced)
| Keybinding | Action |
|-----------|--------|
| `Alt+Shift+;` | Enter service mode |
| *In service mode:* |
| `r` | Flatten workspace tree |
| `f` | Toggle floating/tiling |
| `Backspace` | Close all except current |
| `Alt+Shift+H/J/K/L` | Join windows |
| `Esc` | Exit service mode |

### Layout Management
| Keybinding | Action |
|-----------|--------|
| `Alt+/` | Cycle layouts (tiles/horizontal/vertical) |
| `Alt+,` | Toggle accordion layout |

---

## Layer 2: WezTerm (Terminal Emulator)

### Leader Key System
**Leader**: `Cmd+Space` (ergonomic, two-key combo)

| After Cmd+Space | Action |
|----------------|--------|
| `S` | Save session (Resurrect) |
| `R` | Load session (Resurrect) |
| `D` | Delete session |
| `w` | Switch workspace (zoxide-based) |
| `W` | Create new workspace |

### Tab Management
| Keybinding | Action |
|-----------|--------|
| `Cmd+T` | New tab |
| `Cmd+Q` | Close tab |
| `Cmd+1-9` | Go to tab 1-9 |
| `Cmd+Shift+E` | Rename tab |

### Pane Operations
| Keybinding | Action |
|-----------|--------|
| `Cmd+-` | Split vertical |
| `Cmd+\|` | Split horizontal |
| `Alt+Q` | Close pane (with confirmation) |
| `Alt+X` | Close pane (with confirmation) |
| `Cmd+M` | Toggle pane zoom |

### Navigation (Smart-Splits Integration)
| Keybinding | Action |
|-----------|--------|
| `Ctrl+H/J/K/L` | Navigate panes (seamless with Neovim) |
| `Cmd+H/J/K/L` | Resize panes |

### Scrolling & Search
| Keybinding | Action |
|-----------|--------|
| `Cmd+Shift+PageUp/PageDown` | Scroll by page |
| `Cmd+Shift+B/F` | Scroll by page (vim-style) |
| `Alt+/` | Search current selection |

### Font & Display
| Keybinding | Action |
|-----------|--------|
| `Cmd+=` | Reset font size |
| `Cmd++` | Increase font size |
| `Cmd+-` | Decrease font size |

### Utilities
| Keybinding | Action |
|-----------|--------|
| `Cmd+R` | Reload configuration |
| `Cmd+E` | Edit scrollback in Neovim |
| `Cmd+Shift+D` | Show debug overlay |
| `Cmd+Shift+A` | Show launcher |
| `Cmd+Shift+Space` | Show tab navigator |
| `Cmd+\`` | Rotate panes counterclockwise |
| `Cmd+Shift+\`` | Rotate panes clockwise |

### Copy Mode
| Keybinding | Action |
|-----------|--------|
| `Alt+Ctrl+K` | Enter copy mode |
| *In copy mode - vim-style navigation* |
| `h/j/k/l` | Move cursor |
| `w/b` | Word forward/backward |
| `0/$` | Start/end of line |
| `G/g` | Bottom/top of scrollback |
| `v` | Visual mode |
| `V` | Visual line mode |
| `y` | Yank (copy) |
| `n/N` | Search next/previous |
| `/` | Search |
| `Esc` | Exit copy mode |

### Resize Mode
| Keybinding | Action |
|-----------|--------|
| `Cmd+Shift+R` | Enter resize mode |
| *In resize mode:* |
| `h/j/k/l` or arrows | Resize panes |
| `Esc` | Exit resize mode |

---

## Layer 3: Zellij (Terminal Multiplexer)

See [ZELLIJ_KEYBINDINGS.md](../ZELLIJ_KEYBINDINGS.md) for complete reference.

### Essential Direct Shortcuts
| Keybinding | Action |
|-----------|--------|
| `Ctrl+H/J/K/L` | **Seamless navigation** (Zellij ↔ Neovim) |
| `Ctrl+G` | Lock mode (passthrough) |
| `Ctrl+S` | Scroll mode |
| `Ctrl+Q` | Quit Zellij |

### Leader Key System (Ctrl+A)
After pressing `Ctrl+A`, press:

| Key | Action | Key | Action |
|-----|--------|-----|--------|
| `n` | New pane | `t` | New tab |
| `d` | New pane down | `c` | New tab (tmux) |
| `r` | New pane right | `x` | Close pane |
| `f` | Fullscreen | `z` | Zoom (tmux) |
| `w` | Float toggle | `,` | Rename tab |
| `R` | Resize mode | `M` | Move mode |
| `S` | Session mode | `[` | Scroll mode |
| `h/j/k/l` | Quick focus | `D` | Detach |
| `Space` | Next layout | `Ctrl+A` | Literal Ctrl+A |

---

## Layer 4: Shell (zsh with Oh-My-Zsh)

### Vi Mode (Enabled)
| Keybinding | Mode | Action |
|-----------|------|--------|
| `Esc` / `Ctrl+[` | Insert → Normal | Enter normal mode |
| `i/a/I/A` | Normal → Insert | Insert mode variants |
| `h/j/k/l` | Normal | Navigate command line |
| `w/b/e` | Normal | Word navigation |
| `0/$` | Normal | Start/end of line |
| `dd/D/C` | Normal | Delete operations |

### Essential Readline (Insert Mode)
| Keybinding | Action |
|-----------|--------|
| `Ctrl+A` | Jump to beginning (enabled) |
| `Ctrl+E` | Jump to end (enabled) |
| `Ctrl+U` | Kill line backwards |
| `Ctrl+K` | Kill line forwards |
| `Ctrl+W` | Kill word backwards |

### History Navigation
| Keybinding | Action | Tool |
|-----------|--------|------|
| `Ctrl+P` | History search | **Atuin** (fuzzy search) |
| `Ctrl+R` | **Available** | (Previously reverse search) |
| `Up/Down` | History navigation | Standard zsh |

**Note**: `Ctrl+N` and `Ctrl+P` are freed by Zellij leader system for shell use

### Directory Navigation
| Keybinding | Action | Plugin |
|-----------|--------|--------|
| Type dir name | `cd` to directory | AUTO_CD |
| `z <query>` | Jump to directory | z plugin |
| `d` | Show directory stack | dirstack |

### FZF Integration
| Keybinding | Action |
|-----------|--------|
| `Ctrl+T` | **Available** | (Formerly FZF file search) |
| `Alt+C` | FZF directory change |
| `Ctrl+R` | **Available** | (Atuin uses Ctrl+P) |

### Completion
| Keybinding | Action |
|-----------|--------|
| `Tab` | Trigger completion |
| `Shift+Tab` | Reverse completion |
| `Ctrl+Space` | **Available** | (Zellij moved to leader) |

### Aliases (Notable)
| Alias | Command | Tool |
|-------|---------|------|
| `ls` | `eza` with icons | Modern ls |
| `ll` | `eza -la --header` | Long listing |
| `tree` | `eza --tree` | Tree view |
| `bf` | Butterfish shell | AI shell |

---

## Layer 5: Neovim (LazyVim)

### Leader Keys
| Leader | Key | Purpose |
|--------|-----|---------|
| `<leader>` | `Space` | Primary leader (explicit) |
| `<localleader>` | `,` | Buffer-local mappings |

### File Operations
| Keybinding | Action |
|-----------|--------|
| `<leader>fs` | Save file |
| `<leader>fS` | Save all files |
| `<leader>e` | Toggle file explorer (Neo-tree) |
| `<leader>E` | Reveal file in explorer |

### Window Navigation (Smart-Splits)
| Keybinding | Action |
|-----------|--------|
| `Ctrl+H` | Move cursor left |
| `Ctrl+J` | Move cursor down |
| `Ctrl+K` | Move cursor up |
| `Ctrl+L` | Move cursor right |
| `Ctrl+\` | Move to previous split |

### Window Resizing (Smart-Splits)
| Keybinding | Action |
|-----------|--------|
| `Cmd+H` | Resize left (matches WezTerm!) |
| `Cmd+J` | Resize down |
| `Cmd+K` | Resize up |
| `Cmd+L` | Resize right |

### Buffer Swapping (Smart-Splits)
| Keybinding | Action |
|-----------|--------|
| `<leader><leader>h` | Swap buffer left |
| `<leader><leader>j` | Swap buffer down |
| `<leader><leader>k` | Swap buffer up |
| `<leader><leader>l` | Swap buffer right |

### Claude AI Integration (ClaudeCode Plugin)
| Keybinding | Action |
|-----------|--------|
| `<leader>ac` | Toggle Claude chat |
| `<leader>af` | Focus Claude window |
| `<leader>ar` | Resume Claude session |
| `<leader>aC` | Continue Claude conversation |
| `<leader>am` | Select Claude model |
| `<leader>ab` | Add buffer to Claude context |
| `<leader>as` | Send to Claude (visual mode) |
| `<leader>aa` | Accept AI diff |
| `<leader>ad` | Deny AI diff |

### LazyVim Built-ins (Examples)
| Keybinding | Action |
|-----------|--------|
| `<leader>ff` | Find files (Telescope) |
| `<leader>fg` | Live grep |
| `<leader>fb` | Browse buffers |
| `<leader>bb` | Switch buffer |
| `<leader>bd` | Delete buffer |
| `<leader>qq` | Quit all |

---

## Conflict Resolution History

### Problems Solved

#### 1. Ctrl+N/P Conflict (Zellij vs zsh)
**Problem**: Zellij resize mode (`Ctrl+N`) conflicted with zsh history navigation  
**Solution**: Moved to leader key system (`Ctrl+A R` for resize mode)  
**Result**: `Ctrl+N` and `Ctrl+P` freed for shell use

#### 2. Ctrl+P Conflict (Zellij vs Atuin)
**Problem**: Zellij pane mode used `Ctrl+P`, Atuin uses `Ctrl+P` for history  
**Solution**: Moved Zellij pane mode to leader system (`Ctrl+A p`)  
**Result**: Atuin owns `Ctrl+P` completely

#### 3. Alt+hjkl Conflict (AeroSpace vs Neovim)
**Problem**: AeroSpace window focus intercepts Alt+hjkl before Neovim resizing  
**Solution**: Changed Neovim resizing to `Cmd+hjkl`  
**Result**: Consistency with WezTerm, avoids AeroSpace interception

#### 4. Alt Key Overloading (AeroSpace vs WezTerm)
**Problem**: Both tools wanted Alt for operations  
**Solution**: Alt exclusively for AeroSpace, WezTerm uses Cmd  
**Result**: Clear layer separation, no conflicts

#### 5. WezTerm Leader Ergonomics
**Problem**: `Ctrl+Shift+Space` is a three-key chord (awkward)  
**Solution**: Changed to `Cmd+Space` (two-key combo)  
**Result**: More ergonomic, no Spotlight conflict in terminal

---

## Freed Ctrl Keys

By moving Zellij to leader key system, these Ctrl keys are now available:

| Key | Formerly | Now Available For |
|-----|----------|-------------------|
| `Ctrl+N` | Zellij resize mode | Shell history forward |
| `Ctrl+P` | Zellij pane mode | **Atuin** history search |
| `Ctrl+T` | Zellij tab mode | FZF file search, other uses |
| `Ctrl+Space` | Zellij pane mode | Completion, other uses |
| `Ctrl+O` | Zellij session mode | Vim jumplist |
| `Ctrl+B` | Zellij tmux compat | Tmux mode via `Ctrl+A` |

---

## Quick Lookup by Action

### "I want to create a new..."

| What | How |
|------|-----|
| **Window** | `Alt+Shift+H/J/K/L` (AeroSpace move) |
| **WezTerm Tab** | `Cmd+T` |
| **WezTerm Pane** | `Cmd+-` or `Cmd+\|` |
| **Zellij Tab** | `Ctrl+A` then `t` |
| **Zellij Pane** | `Ctrl+A` then `n/d/r` |
| **Neovim Split** | `:split` or `:vsplit` |

### "I want to navigate..."

| What | How |
|------|-----|
| **Between windows** | `Alt+H/J/K/L` |
| **Between workspaces** | `Alt+1/D/C/O/T` |
| **Between WezTerm tabs** | `Cmd+1-9` |
| **Between WezTerm panes** | `Ctrl+H/J/K/L` |
| **Between Zellij panes** | `Ctrl+H/J/K/L` (seamless!) |
| **Between Neovim splits** | `Ctrl+H/J/K/L` (seamless!) |
| **Shell history** | `Ctrl+P` (Atuin) |

### "I want to resize..."

| What | How |
|------|-----|
| **Windows** | `Alt+Shift+=/-` |
| **WezTerm panes** | `Cmd+H/J/K/L` |
| **Zellij panes** | `Ctrl+A` then `R`, then `hjkl` |
| **Neovim splits** | `Cmd+H/J/K/L` |

### "I want to close/quit..."

| What | How |
|------|-----|
| **Window** | `Cmd+Q` (app-level) |
| **WezTerm tab** | `Cmd+Q` |
| **WezTerm pane** | `Alt+Q` or `Alt+X` |
| **Zellij tab** | `Ctrl+A` then `x` (in tab mode) |
| **Zellij pane** | `Ctrl+A` then `x` |
| **Zellij completely** | `Ctrl+Q` |
| **Neovim** | `:q` or `<leader>qq` |

---

## Ergonomic Tips

### 1. Caps Lock → Escape (Recommended)
**Why**: Vim users hit Escape frequently; Caps Lock is in a prime location  
**How**: macOS System Settings → Keyboard → Modifier Keys → Caps Lock → Escape

### 2. Leader Keys are Your Friend
- **Zellij**: `Ctrl+A` - One key to remember, then mnemonic letters
- **Neovim**: `Space` - Thumb-accessible, discoverable with which-key
- **WezTerm**: `Cmd+Space` - Session management, less frequent

### 3. Muscle Memory Optimization
- **Ctrl+hjkl** works EVERYWHERE for navigation
- **Cmd+hjkl** works EVERYWHERE for resizing
- Same patterns = faster learning

### 4. Use Locked Mode
- In Zellij, press `Ctrl+G` to enter locked mode
- All keys go to underlying application
- Useful when running another TUI that needs full keyboard access
- Press `Ctrl+G` again to exit

### 5. Memorize Three Leader Keys
1. **Ctrl+A** (Zellij) - Terminal multiplexer operations
2. **Space** (Neovim) - Editor operations
3. **Cmd+Space** (WezTerm) - Session operations

---

## Testing Checklist

Use this to verify everything works:

### AeroSpace
- [ ] `Alt+H/J/K/L` focuses windows
- [ ] `Alt+1/D/C/O/T` switches workspaces
- [ ] `Alt+Shift+H/J/K/L` moves windows

### WezTerm
- [ ] `Cmd+T` creates tab
- [ ] `Ctrl+H/J/K/L` navigates panes
- [ ] `Cmd+H/J/K/L` resizes panes
- [ ] `Cmd+Space` then `S` saves session

### Zellij
- [ ] `Ctrl+H/J/K/L` navigates panes
- [ ] `Ctrl+A` then `n` creates pane
- [ ] `Ctrl+A` then `t` creates tab
- [ ] `Ctrl+G` locks/unlocks
- [ ] `Ctrl+S` enters scroll mode
- [ ] No conflicts with shell (try `Ctrl+N`, `Ctrl+P`, `Ctrl+T`)

### Neovim
- [ ] `Ctrl+H/J/K/L` navigates splits (and crosses to Zellij)
- [ ] `Cmd+H/J/K/L` resizes splits (not intercepted by AeroSpace)
- [ ] `Space` leader works for all commands
- [ ] `<leader>e` toggles file explorer

### Shell (zsh)
- [ ] `Ctrl+P` opens Atuin history
- [ ] `Ctrl+A` / `Ctrl+E` jump to line start/end
- [ ] `Ctrl+R` is available
- [ ] `Ctrl+N` is available (no Zellij conflict)

---

## Configuration Files

| Tool | Config Location |
|------|----------------|
| AeroSpace | `~/nix/dotfiles/config/aerospace/aerospace.toml` |
| WezTerm | `~/nix/dotfiles/config/wezterm/wezterm.lua` |
| WezTerm Keys | `~/nix/dotfiles/config/wezterm/keybinds.lua` |
| Zellij | `~/nix/modules/home-manager/zellij/config.kdl` |
| Zellij Layouts | `~/nix/modules/home-manager/zellij/layouts/` |
| Neovim Options | `~/nix/dotfiles/config/nvim/lua/config/options.lua` |
| Neovim Keymaps | `~/nix/dotfiles/config/nvim/lua/config/keymaps.lua` |
| Neovim Smart-Splits | `~/nix/dotfiles/config/nvim/lua/plugins/smart-splits/init.lua` |
| zsh Bindings | `~/nix/dotfiles/config/zsh/rc/binds.zsh` |
| zsh Atuin | `~/nix/dotfiles/config/zsh/rc/atuin.zsh` |

---

## Summary Philosophy

**One Modifier Per Layer**
- Alt = Window manager
- Cmd = Terminal emulator
- Ctrl = Multiplexer + Editor navigation
- Leader keys = Extended operations

**Consistency is King**
- `Ctrl+hjkl` navigation works everywhere
- `Cmd+hjkl` resizing works everywhere
- Same patterns across all tools

**Zero Conflicts**
- Each tool owns its modifier space
- Leader keys eliminate Ctrl conflicts
- Alt freed for AeroSpace exclusively

**Ergonomic First**
- Two-key combos preferred over three
- Common actions need fewer keystrokes
- Mnemonic keys after leaders

**Muscle Memory Wins**
- Learn once, use everywhere
- Seamless transitions between tools
- Predictable behavior

---

**Result**: A conflict-free, ergonomic, cognitively optimized keyboard-driven development environment that leverages muscle memory across all tools.
