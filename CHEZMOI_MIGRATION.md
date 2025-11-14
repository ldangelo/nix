# Chezmoi to Nix Migration

## Overview

Successfully migrated all configuration files from chezmoi to the Nix repository, making the entire system configuration self-contained in a single git repository.

## What Was Migrated

### From `~/.local/share/chezmoi/` to `dotfiles/`

All configuration files have been copied into the nix repository under `dotfiles/`:

```
dotfiles/
├── config/              # All .config directory contents
│   ├── aerospace/       # Window manager
│   ├── borders/         # Window borders (managed by borders module)
│   ├── doom/            # Doom Emacs config
│   ├── jj/              # Jujutsu config
│   ├── kanata/          # Keyboard remapper
│   ├── msmtp/           # SMTP client
│   ├── neomutt/         # Email client
│   ├── nvim/            # Neovim config (40+ files)
│   ├── skhd/            # Hotkey daemon
│   ├── userscripts/     # Browser userscripts
│   ├── wezterm/         # Terminal config (6 files)
│   ├── yabai/           # Window manager
│   └── zsh/             # Zsh config (20+ files)
├── emacs-profile        # Emacs profile selector
├── emacs-profiles.el    # Emacs profiles config
├── gitconfig            # Git config (now managed by git.nix)
├── ideavimrc            # IdeaVim config
├── mailrc               # Mail config
├── mbsyncrc             # Mailbox sync config
└── oh-my-zsh/           # Oh My Zsh installation
```

## Home-Manager Modules Created

### Native Program Configurations
- `modules/home-manager/git.nix` - Git configuration using programs.git
- `modules/home-manager/jujutsu.nix` - Jujutsu configuration using programs.jujutsu
- `modules/home-manager/nvim/` - Neovim configuration
- `modules/home-manager/wezterm.nix` - WezTerm configuration

### Dotfiles Management
- `modules/home-manager/dotfiles-chezmoi.nix` - Manages all dotfiles from `dotfiles/` directory

## Key Benefits

1. **Self-Contained**: No dependency on separate chezmoi repository
2. **Version Controlled**: All configs in git with full history
3. **Declarative**: Nix manages all file placements
4. **Reproducible**: Clone repo and build - everything is configured
5. **No Chezmoi Needed**: One less tool to maintain

## Configuration Locations

### Managed by Home-Manager Modules
- Git: `programs.git` in `git.nix`
- Jujutsu: `programs.jujutsu` in `jujutsu.nix`
- Direnv: `programs.direnv` in `programs.nix`
- FZF: `programs.fzf` in `programs.nix`
- Starship: `programs.starship` in `programs.nix`

### Managed as File Symlinks
- All other configs: `xdg.configFile` and `home.file` in `dotfiles-chezmoi.nix`

## Usage

### Apply Configuration
```bash
cd /Users/ldangelo/Development/nix
darwin-rebuild switch --flake .#ldangelo
```

### Update a Config File
1. Edit files in `dotfiles/`
2. Rebuild: `darwin-rebuild switch --flake .#ldangelo`
3. Commit changes: `git add dotfiles/ && git commit`

### Add New Config
1. Add to `dotfiles/config/` or `dotfiles/`
2. Update `modules/home-manager/dotfiles-chezmoi.nix`
3. Rebuild and test

## Migration Path

**Before:**
- Configs in `~/.local/share/chezmoi/`
- Managed with `chezmoi apply`
- Separate git repo for dotfiles

**After:**
- Configs in `/Users/ldangelo/Development/nix/dotfiles/`
- Managed with `darwin-rebuild switch`
- Single git repo for entire system

## Cleanup (Optional)

After verifying everything works:

```bash
# Backup chezmoi repo (optional)
cp -r ~/.local/share/chezmoi ~/chezmoi-backup

# Remove chezmoi (optional, after thorough testing)
# chezmoi purge
# brew uninstall chezmoi
```

## Notes

- **Spacevim**: Disabled due to conflict with nvim config
- **Borders**: Managed by dedicated `borders` module
- **Direnv**: Managed by `programs.direnv`, not as file symlink
- **Embedded Git Repos**: Cleaned up (doom/modules/editor/meow)

## Conflicts Resolved

1. `borders` - Already had dedicated module
2. `direnv` - Already configured via programs.direnv
3. `spacevim` - Disabled (conflicts with nvim)
4. `nvim` - Using simplified approach without programs.neovim (structuredAttrs issues)
