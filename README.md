# nix-darwin System Configuration

Personal macOS system configuration using Nix flakes, nix-darwin, and home-manager for fully declarative system management.

## Overview

This repository contains the complete system configuration for a macOS development workstation (aarch64-darwin). It manages:

- System-level settings (nix-darwin)
- User environment and dotfiles (home-manager)
- Package installations (Nix + Homebrew)
- Application configurations
- Secrets management (sops-nix)
- Development tools and language runtimes

## Quick Start

### Prerequisites

Install Nix using the Determinate Nix Installer:

```bash
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

See: https://docs.determinate.systems/

### Initial Setup

```bash
# Clone this repository
git clone <repo-url> ~/nix
cd ~/nix

# Deploy the configuration
just deploy
```

### Common Commands

```bash
# Deploy system configuration
just deploy                    # Standard deployment
just deploy-nc                 # Deploy without cache (useful after flake changes)
just deploy-rebuild            # Force complete rebuild
just debug                     # Deploy with verbose output for troubleshooting

# Maintenance
just up                        # Update all flake inputs
just upp i=home-manager       # Update specific input
just history                   # View configuration generations
just clean                     # Remove old generations (>7 days)
just gc                        # Garbage collect unused packages (>14 days)
just real-clean               # Combined cleanup
```

## Architecture

### Technology Stack

**Core**:
- [Nix](https://nixos.org/) - Package manager and build system
- [nix-darwin](https://github.com/LnL7/nix-darwin) - macOS system configuration
- [home-manager](https://github.com/nix-community/home-manager) - User environment management
- [flake-parts](https://flake.parts/) - Modular flake organization

**Key Features**:
- Declarative system configuration
- Reproducible builds
- Atomic upgrades and rollbacks
- Secrets management via sops-nix
- Catppuccin theming across all tools

### Directory Structure

```
nix/
├── flake.nix                  # Main flake configuration and inputs
├── flake.lock                 # Locked dependency versions
├── Justfile                   # Command runner recipes
│
├── modules/
│   ├── darwin/               # System-level configuration
│   │   ├── default.nix       # Entry point for darwin modules
│   │   ├── system.nix        # macOS system defaults
│   │   ├── homebrew.nix      # Homebrew packages and casks
│   │   ├── services.nix      # Launchd services (kanata, karabiner)
│   │   ├── sops.nix          # Secrets management configuration
│   │   └── pam.nix           # Touch ID/Apple Watch for sudo
│   │
│   └── home-manager/         # User environment configuration
│       ├── default.nix       # Entry point for home-manager modules
│       ├── packages.nix      # Nix packages (migrated from Brewfile)
│       ├── shell.nix         # Zsh configuration
│       ├── git.nix           # Git settings
│       ├── programs.nix      # Program configurations (fzf, direnv, starship)
│       ├── nvim/             # Neovim configuration
│       ├── emacs/            # Emacs configuration
│       ├── zellij/           # Zellij terminal multiplexer
│       └── qutebrowser/      # Qutebrowser browser
│
├── overlays/                 # Custom package overlays
│   ├── default.nix
│   └── sketchybar-lua/       # Custom sketchybar-lua package
│
├── dotfiles/                 # Legacy dotfiles (transitioning to home-manager)
├── secrets/                  # sops-nix encrypted secrets
│
└── docs/                     # Design documentation
    ├── PRD/                  # Product Requirements Documents
    └── TRD/                  # Technical Requirements Documents
```

## Configuration Management

### Adding Packages

**Via Nix** (preferred):
```nix
# Edit modules/home-manager/packages.nix
home.packages = with pkgs; [
  # Add your package here
  new-package
];
```

**Via Homebrew** (for unavailable Nix packages):
```nix
# Edit modules/darwin/homebrew.nix
homebrew.brews = [
  "new-formula"
];

homebrew.casks = [
  "new-application"
];
```

Then deploy: `just deploy`

### Modifying System Settings

```nix
# Edit modules/darwin/system.nix
system.defaults = {
  NSGlobalDomain = {
    AppleShowAllExtensions = true;
    # ... other settings
  };

  dock = {
    autohide = true;
    # ... other settings
  };
};
```

Then deploy: `just deploy`

### Managing Secrets

Secrets are encrypted using sops-nix with age encryption:

```bash
# Edit secrets (opens in your $EDITOR)
./edit_secrets.sh

# Secrets are stored in secrets/secrets.yaml (encrypted)
# Keys are automatically extracted via sops-nix on system activation
```

Reference secrets in configuration:
```nix
home.file.".envrc".text = ''
  export API_KEY="$(cat /run/secrets/api_key 2>/dev/null || echo "")"
'';
```

## Development Tools

### Editors

- **Neovim**: LazyVim configuration with LSP, Treesitter, and custom keymaps
- **Emacs**: Standalone installation (via Homebrew) with Nix-managed dependencies
- **VSCode**: With Catppuccin theme and essential extensions

### Terminal Environment

- **Terminal Emulator**: Ghostty (fast, native macOS)
- **Multiplexer**: Zellij with seamless Neovim navigation (Ctrl+hjkl)
- **Shell**: Zsh with Oh My Zsh
- **Prompt**: Starship (fast, customizable)
- **History**: Atuin (SQLite-based, searchable)
- **Directory Navigation**: Zoxide (smart cd)

**Key Integrations**:
- Zellij-Neovim navigation via vim-zellij-navigator
- No keybinding conflicts with Aerospace window manager
- See `ZELLIJ_KEYBINDINGS.md` for complete reference

### Version Control

- Git + GitHub CLI (`gh`)
- GitLab CLI (`glab`)
- Jujutsu (`jj`) - Modern VCS
- Git Town - Workflow automation
- Graphite CLI - Stacked PRs
- Lazygit - Terminal UI

### Languages & Runtimes

Managed via Nix:
- Python 3 + uv/pipx
- Ruby + rbenv
- Rust + Cargo
- Elixir
- Java (OpenJDK 21)
- .NET SDK 8
- PostgreSQL

Managed via Homebrew:
- Node.js (via nvm)

### Cloud & Infrastructure

- AWS CLI v2
- kubectl + Helm
- Terraform
- Docker
- Temporal CLI
- Kubernetes tooling (kubeconform, etc.)

## System Features

### macOS Integration

- Touch ID and Apple Watch for sudo authentication
- Optimized key repeat rates for vim-style navigation
- Custom launchd services (kanata, karabiner)
- Dock and Finder customizations

### Theming

Consistent Catppuccin theme across:
- Neovim
- VSCode
- Zsh (syntax highlighting)
- Qutebrowser
- Atuin
- Bat
- Btop
- FZF
- Lazygit
- Starship
- WezTerm

## Troubleshooting

### Build Failures

```bash
# Clear cache and retry
just deploy-nc

# Verbose output
just debug

# Check specific error logs
nix log <failed-derivation-path>
```

### Rollback to Previous Generation

```bash
# List available generations
just history

# Rollback to previous
sudo nix-env --rollback --profile /nix/var/nix/profiles/system
sudo /nix/var/nix/profiles/system/activate

# Or switch to specific generation
sudo nix-env --switch-generation <n> --profile /nix/var/nix/profiles/system
```

### Clean Up Disk Space

```bash
# Remove old generations (>7 days) and garbage collect (>14 days)
just real-clean

# Or individually:
just clean    # Remove old generations
just gc       # Garbage collect
```

### Common Issues

**"permission denied" errors**:
- Check file ownership: `ls -la /nix/var/nix/profiles/`
- Ensure you have sudo access

**Homebrew conflicts**:
- Nix and Homebrew can coexist
- Prefer Nix packages when available
- Use Homebrew only for packages unavailable in nixpkgs

**Flake lock conflicts**:
- Update inputs: `just up`
- Or update specific input: `just upp i=nixpkgs`

## Migration Guides

- **`BREWFILE_MIGRATION.md`**: Historical package migration from Brewfile to Nix
- **`CHEZMOI_MIGRATION.md`**: Dotfiles migration from chezmoi to home-manager
- **`ZELLIJ_KEYBINDINGS.md`**: Keybinding reference and Aerospace integration

## Development Workflow

### Making Changes

1. Edit configuration files in `modules/`
2. Test locally: `just deploy-nc`
3. Verify changes work as expected
4. Commit with descriptive message
5. Push to repository

### Testing Before Deploy

```bash
# Validate flake
nix flake check

# Evaluate without building
nix eval .#darwinConfigurations."Leos-MacBook-Pro".config.system.build.toplevel

# Dry run (shows what would change)
darwin-rebuild build --flake .
```

### Best Practices

- **Read before modifying**: Use `cat` or your editor to review existing config
- **Preserve patterns**: Keep commented sections (they're there for a reason)
- **Test incrementally**: Make small changes, deploy, verify
- **Document changes**: Add comments for non-obvious configurations
- **Use just commands**: Prefer `just deploy` over raw `darwin-rebuild`

## Resources

### Nix Learning

- [Zero to Nix](https://zero-to-nix.com/) - Beginner-friendly introduction
- [Nix.dev](https://nix.dev/) - Practical tutorials
- [NixOS Wiki](https://nixos.wiki/) - Community documentation

### Configuration References

- [nix-darwin Options](https://daiderd.com/nix-darwin/manual/index.html)
- [home-manager Options](https://nix-community.github.io/home-manager/options.xhtml)
- [Nix Flakes Manual](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html)

### Community

- [NixOS Discourse](https://discourse.nixos.org/)
- [Nix Matrix Channels](https://matrix.to/#/#nix:nixos.org)
- [r/NixOS](https://www.reddit.com/r/NixOS/)

## Claude Code Integration

This repository is optimized for use with Claude Code. See `CLAUDE.md` for:
- Context management strategies
- Agent mesh integration patterns
- Project-specific workflows
- Memory management priorities

Available integrations:
- Emacs IDE integration via `claude-code-ide.el`
- LSP navigation (xref)
- Tree-sitter syntax analysis
- Imenu symbol navigation

## License

MIT License - See `LICENSE` file for details

## Acknowledgments

Built with:
- [Determinate Nix Installer](https://github.com/DeterminateSystems/nix-installer)
- [nix-darwin](https://github.com/LnL7/nix-darwin) by @LnL7
- [home-manager](https://github.com/nix-community/home-manager)
- [Catppuccin](https://github.com/catppuccin/catppuccin) theme
- Inspired by the broader Nix community

---

**Current Configuration Version**: 25.11.3bda9f6
**Last Updated**: 2026-01-15
