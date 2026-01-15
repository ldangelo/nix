# Zellij Keybinding Changes

## Summary

All Alt key bindings have been removed from Zellij to eliminate conflicts with Aerospace window manager. The configuration now uses a hybrid approach with:
- **Seamless Neovim navigation** via vim-zellij-navigator (Ctrl+hjkl)
- **Ctrl-based modal system** for advanced operations (Ctrl+Space for pane mode)
- **Ctrl+Shift quick shortcuts** for common actions
- **Atuin integration** - Ctrl+P now works for shell history (pane mode moved to Ctrl+Space)

## What Changed

### Removed (Conflicted with Aerospace)
- `Alt+hjkl` / `Alt+arrows` - Navigation (now exclusively Aerospace)
- `Alt+f` - Toggle floating (now exclusively Aerospace fullscreen)
- `Alt+o/i` - Move tabs (conflicted with Aerospace workspaces)
- `Alt+n` - New pane (conflicted with Aerospace workspace N)
- `Alt+p` - Toggle pane group (conflicted with Aerospace workspace P)
- `Alt+Shift+p` - Toggle group marking (conflicted with Aerospace)
- `Alt+[/]` - Swap layouts (avoided future conflicts)
- `Alt++/-/=` - Resize (avoided future conflicts)

### Added (New Quick Shortcuts)

**Pane Operations:**
- `Ctrl+Shift+n` - New pane
- `Ctrl+Shift+d` - New pane down
- `Ctrl+Shift+r` - New pane right
- `Ctrl+Shift+x` - Close focused pane
- `Ctrl+Shift+f` - Toggle fullscreen
- `Ctrl+Shift+w` - Toggle floating panes
- `Ctrl+Shift+e` - Toggle pane embed/floating

**Tab Operations:**
- `Ctrl+Shift+t` - New tab
- `Ctrl+Shift+q` - Close tab
- `Ctrl+Shift+Left` - Previous tab
- `Ctrl+Shift+Right` - Next tab
- `Ctrl+Shift+Space` - Toggle last tab

**Quick Navigation:**
- `Ctrl+Shift+h` - Focus left pane
- `Ctrl+Shift+j` - Focus down pane
- `Ctrl+Shift+k` - Focus up pane
- `Ctrl+Shift+l` - Focus right pane

**Layout:**
- `Ctrl+Shift+[` - Previous layout
- `Ctrl+Shift+]` - Next layout

### Preserved (No Changes)

**Modal System (Ctrl-based):**
- `Ctrl+Space` → Pane mode (changed from Ctrl+P to avoid Atuin conflict)
- `Ctrl+t` → Tab mode
- `Ctrl+n` → Resize mode
- `Ctrl+h` → Move mode (removed, use pane mode instead)
- `Ctrl+s` → Scroll mode
- `Ctrl+b` → Tmux compatibility mode
- `Ctrl+o` → Session mode
- `Ctrl+g` → Locked mode (pass-through)
- `Ctrl+q` → Quit

**Within-Mode Bindings:**
All hjkl navigation and single-key commands within modes remain unchanged.

## ⭐ Seamless Neovim Integration (NEW!)

### The Magic: Ctrl+hjkl Works Everywhere!

**`Ctrl+h/j/k/l`** now provides seamless navigation across Zellij and Neovim:

- **In a shell/app**: Moves between Zellij panes
- **Inside Neovim**: Automatically navigates Neovim splits
- **In ANY Zellij mode**: Always switches panes (except locked mode)
- **No mode switching needed** - The plugin intelligently detects context!

**Powered by:**
- **Zellij**: [vim-zellij-navigator v0.3.0](https://github.com/hiasr/vim-zellij-navigator)
- **Neovim**: [smart-splits.nvim](https://github.com/mrjones2014/smart-splits.nvim)

### How It Works

Press `Ctrl+h` to move left:
- **In shell**: Focuses left Zellij pane
- **In Neovim**: Moves to left Neovim split
- **In any mode** (pane, tab, resize, etc.): Always navigates panes
- **At edge**: Stays in place (no wrapping)

**Key Feature**: `Ctrl+hjkl` works in ALL Zellij modes except locked mode. This means you can always navigate between panes, even when you're in pane mode, tab mode, resize mode, etc. You never get "stuck" unable to switch panes!

**Resizing** (Neovim only):
- `Alt+hjkl` resizes Neovim splits
- Works when Neovim has focus (Neovim captures keys before Aerospace)

## Atuin Integration (Shell History)

### Ctrl+P Conflict Resolution

**Problem**: Atuin uses `Ctrl+P` for history navigation (up arrow equivalent), which conflicted with Zellij's pane mode.

**Solution**: Moved Zellij pane mode to **`Ctrl+Space`** instead.

**Why Ctrl+Space?**
- ✅ Industry standard for leader/prefix keys (tmux, Spacemacs)
- ✅ No conflicts with shell commands or Atuin
- ✅ Ergonomic and easy to press
- ✅ Keeps your Atuin muscle memory intact

**Your Atuin workflow now works perfectly**:
- `Ctrl+P` → Atuin history (up)
- `Ctrl+N` → Atuin history (down)
- `Ctrl+R` → Atuin search
- No need to enter locked mode!

## New Workflow

### System-Level Navigation (Aerospace)
Use `Alt+hjkl` to move between application windows - this now works everywhere without conflicts.

### Terminal-Level Navigation (Zellij)
Choose your preferred method:

**Option 1: Quick Shortcuts (One Keystroke)**
- `Ctrl+Shift+hjkl` - Jump between panes immediately
- `Ctrl+Shift+n` - Create new pane immediately
- `Ctrl+Shift+f` - Toggle fullscreen immediately

**Option 2: Modal System (More Operations)**
- `Ctrl+Space` then `hjkl` - Navigate panes
- `Ctrl+Space` then `n` - New pane
- `Ctrl+Space` then `f` - Toggle fullscreen
- `Esc` or `Enter` - Return to normal mode

### Special Cases
- **Locked Mode**: `Ctrl+g` - Pass all keys through to underlying application (for nvim, htop, etc.)
- **Tmux Mode**: `Ctrl+b` - Tmux-compatible keybindings

## Conflict Resolution

| Key Combination | Layer | Purpose |
|----------------|-------|---------|
| `Alt+hjkl` | System (Aerospace) | Window focus navigation |
| `Ctrl+hjkl` | **Seamless (Zellij + Neovim)** | **Smart navigation across panes and splits** |
| `Ctrl+Shift+hjkl` | Terminal (Zellij) | Direct pane focus (no Neovim integration) |
| `Alt+hjkl` | Application (Neovim) | Split resizing |
| `Ctrl+Space/t/n/s/b/o` | Terminal (Zellij) | Mode switches |

## Testing Checklist

### Seamless Navigation (Priority)
- [ ] **In shell**: `Ctrl+hjkl` navigates between Zellij panes
- [ ] **In Neovim**: `Ctrl+hjkl` navigates between Neovim splits
- [ ] **Shell to Neovim**: Create 2 panes (shell + nvim), use `Ctrl+h/l` to switch between them
- [ ] **Inside Neovim**: Create splits with `:split` and `:vsplit`, use `Ctrl+hjkl` to navigate
- [ ] **In pane mode**: Press `Ctrl+Space`, then `Ctrl+hjkl` should still navigate panes (not just hjkl)
- [ ] **In tab mode**: Press `Ctrl+t`, then `Ctrl+hjkl` should still navigate panes
- [ ] **In resize mode**: Press `Ctrl+n`, then `Ctrl+hjkl` should still navigate panes (not resize)
- [ ] **Seamless feel**: Navigation feels smooth and predictable in both contexts and all modes

### System Integration
- [ ] `Alt+hjkl` moves between application windows (Aerospace, not captured by Zellij)
- [ ] `Alt+f` toggles fullscreen (Aerospace, works everywhere)
- [ ] Aerospace workspace switches work (`Alt+1`, `Alt+d`, `Alt+c`, etc.)

### Zellij Quick Shortcuts
- [ ] `Ctrl+Shift+hjkl` navigates between Zellij panes (direct, no Neovim integration)
- [ ] `Ctrl+Shift+n` creates new Zellij pane
- [ ] `Ctrl+Shift+t` creates new tab
- [ ] `Ctrl+Shift+f` toggles fullscreen

### Modal System
- [ ] `Ctrl+Space` then `hjkl` navigates in pane mode
- [ ] `Ctrl+t` then `hjkl` navigates tabs
- [ ] `Ctrl+n` then `hjkl` resizes panes
- [ ] `Esc` or `Enter` exits modes

### Neovim Resizing
- [ ] Inside Neovim: `Alt+hjkl` resizes splits
- [ ] Resizing works when Neovim has focus

### Atuin Integration
- [ ] `Ctrl+P` triggers Atuin history (up) - not captured by Zellij
- [ ] `Ctrl+N` triggers Atuin history (down) - not captured by Zellij
- [ ] `Ctrl+R` triggers Atuin search - works normally
- [ ] Atuin keybindings work in all panes without entering locked mode

### Edge Cases
- [ ] `Ctrl+g` enters locked mode (all keys pass through)
- [ ] No double-triggers or unexpected behavior
- [ ] No conflicts between different keybinding layers

## Files Modified

- `modules/home-manager/zellij/config.kdl` - Added vim-zellij-navigator integration and keybindings
- `dotfiles/config/nvim/lua/plugins/smart-splits/init.lua` - Enabled multiplexer integration
- `dotfiles/config/nvim/lua/config/keymaps.lua` - Already configured with Ctrl+hjkl navigation

## Configuration Location

The active Zellij configuration is loaded from:
```
/Users/ldangelo/.config/zellij/config.kdl
```

## Rebuilding After Changes

If you modify the nix configuration, rebuild with:
```bash
just deploy  # or sudo darwin-rebuild switch --flake .#Leos-MacBook-Pro
```

## Reverting Changes

If needed, revert by:
```bash
git checkout HEAD~1 modules/home-manager/zellij/config.kdl
just deploy
```

## Future Enhancements

Consider these optional improvements:

1. **WezTerm Cleanup**: Remove Alt bindings from WezTerm to match Zellij pattern
2. **Cheat Sheet**: Create visual reference for muscle memory
3. **Fine-tuning**: Adjust quick shortcuts based on usage patterns after 2-4 weeks
4. **Custom Layouts**: Optimize default layout for your workflow

## Reference

- Implementation Plan: `~/.claude/plans/tranquil-crunching-sketch.md`
- Aerospace Config: `dotfiles/config/aerospace/aerospace.toml`
- Neovim Keymaps: `dotfiles/config/nvim/lua/config/keymaps.lua`
- WezTerm Keybinds: `dotfiles/config/wezterm/keybinds.lua`
