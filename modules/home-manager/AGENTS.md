---
disclose: always
summary: Home Manager module structure, user environment conventions, editor config locations, and Pi extension workflow
triggers: [home-manager, packages, shell, git, nvim, emacs, pi-agent, pi-extensions, dotfiles]
---

# Home Manager Module AGENTS.md

User environment and dotfiles. Loaded by home-manager.

## Structure

| File/Dir | Purpose |
|----------|---------|
| `packages.nix` | Nix-installed packages (not Homebrew) |
| `shell.nix` | Zsh, starship, atuin, direnv |
| `git.nix` | Git config and credentials |
| `programs.nix` | fzf, zellij, qutebrowser, etc. |
| `nvim/` | Neovim/LazyVim configuration |
| `emacs/` | Emacs configuration |
| `sketchybar/` | Status bar configuration |
| `tmux/` | Tmux configuration |
| `pi-extensions/` | Pi coding agent extensions and skills |
| `pizauth/` | Pi Z Auth |
| `streamdeck/` | Elgato Stream Deck config |
| `borders/` | Window borders config |
| `dotfiles/` | Legacy dotfiles (migrating to home-manager) |

## Conventions

- New programs go in `packages.nix` (Nix) or `shell.nix` (shell-level)
- Editor configs live in their own subdirectories (`nvim/`, `emacs/`)
- Pi agent extensions go in `pi-extensions/` — follow the pattern in existing ones

## Adding a Neovim Plugin

1. Add to `nvim/lazy.nix` under the appropriate `plugins` section
2. Add any config to `nvim/config/` or inline in lazy.nix
3. Restart Neovim to pick up changes

## Adding a Pi Extension

1. Create directory in `pi-extensions/<name>/`
2. Add `SKILL.md` with skill description
3. Add implementation files (TypeScript, Python, etc.)
4. Register in `pi-extensions/default.nix` if needed
