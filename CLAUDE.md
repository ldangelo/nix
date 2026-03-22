# nix-darwin System Configuration

Get the task management instructions.

READ AGENTS.md

## Project Overview

Personal macOS system configuration using Nix flakes, nix-darwin, and home-manager. Manages development environment, applications, dotfiles, and system settings declaratively across a complete macOS workstation setup.

**Installation Method**: Determinate Nix Installer (<https://docs.determinate.systems/>)
**Target System**: macOS (aarch64-darwin)
**User**: ldangelo

## Quick Reference

### Common Commands

```bash
# Deploy system configuration
just deploy                    # Standard deployment
just deploy-nc                 # Deploy without cache
just deploy-rebuild            # Force rebuild
just debug                     # Deploy with verbose output

# Maintenance
just up                        # Update all flake inputs
just upp i=<input>            # Update specific input
just history                   # View configuration generations
just clean                     # Remove old generations (>7 days)
just gc                        # Garbage collect (>14 days)
```

### Key Locations

- **Flake**: `/Users/ldangelo/nix/flake.nix`
- **Darwin Config**: `/Users/ldangelo/nix/modules/darwin/`
- **Home Manager**: `/Users/ldangelo/nix/modules/home-manager/`
- **Secrets**: `/Users/ldangelo/nix/secrets/` (sops-nix encrypted)
- **Dotfiles**: `/Users/ldangelo/nix/dotfiles/`

## Architecture

### Configuration Structure

```
nix/
├── flake.nix                  # Main flake configuration
├── Justfile                   # Command runner recipes
├── modules/
│   ├── darwin/               # System-level configuration
│   │   ├── default.nix
│   │   ├── homebrew.nix      # Homebrew packages/casks
│   │   ├── services.nix      # Launchd services
│   │   ├── system.nix        # macOS defaults
│   │   ├── sops.nix          # Secrets management
│   │   └── pam.nix           # Touch ID/Apple Watch sudo
│   └── home-manager/         # User environment
│       ├── default.nix
│       ├── packages.nix      # Nix packages (migrated from Brewfile)
│       ├── shell.nix         # Zsh configuration
│       ├── git.nix           # Git settings
│       ├── nvim/             # Neovim configuration
│       ├── emacs/            # Emacs configuration
│       ├── tmux/             # tmux terminal multiplexer
│       └── qutebrowser/      # Qutebrowser browser
├── overlays/
│   ├── sketchybar-lua/       # Custom package overlay
│   └── default.nix
├── dotfiles/                  # Managed dotfiles
├── secrets/                   # sops-nix encrypted secrets
└── docs/                      # Design documents
    ├── PRD/                   # Product Requirements
    └── TRD/                   # Technical Requirements
```

### Technology Stack

**Core Infrastructure**:

- Nix flakes (unstable channel)
- nix-darwin (system management)
- home-manager (user environment)
- flake-parts (modular flake organization)

**Key Integrations**:

- sops-nix (secrets encryption)
- catppuccin (theming across tools)
- emacs-overlay (latest Emacs builds)
- LazyVim-module (Neovim configuration)
- nix-search-tv (package search UI)

### Development Tools

**Primary Editor Stack**:

- Neovim (LazyVim configuration)
- Emacs (managed via nix, config separate)
- VSCode (with Catppuccin theme)

**Terminal Environment**:

- tmux multiplexer with seamless Neovim navigation (Ctrl+hjkl via vim-tmux-navigator)
- iTerm terminal emulator
- Starship prompt
- Zsh with Oh My Zsh
- Atuin (shell history)
- Zoxide (smart cd)

**Version Control**:

- Git + GitHub CLI (gh)
- GitLab CLI (glab)
- Jujutsu (jj)
- Git Town (workflow automation)
- Graphite CLI (stacked PRs)
- Lazygit (TUI)

**Cloud & Infrastructure**:

- AWS CLI v2
- kubectl + Helm
- Terraform
- Docker
- Temporal
- Kubernetes tooling

**Languages & Runtimes**:

- Node.js (via nvm from Homebrew)
- Python 3 + uv/pipx
- Ruby + rbenv
- Rust + Cargo
- Elixir
- Java (OpenJDK 21)
- .NET SDK 8
- PostgreSQL

## Recent Development Patterns

### Active Focus Areas (Last 3 Months)

1. **Terminal Multiplexer Migration**: Migrated back to tmux (Sidecar compatibility)
   - Ctrl+Space prefix, vi mode, Catppuccin theme
   - Seamless Neovim-tmux navigation via vim-tmux-navigator (Ctrl+hjkl)
   - Layout scripts in `~/.local/bin/tmux-{simple,editor,dev,monitor,claude}`

2. **Editor Configuration Evolution**: Doom Emacs → Standalone Emacs + Neovim dual setup
   - Removed Doom Emacs from repository
   - Install Emacs via Homebrew, manage config through Nix
   - Enhanced Neovim with smart-splits and LazyVim

3. **Package Management Modernization**: Brewfile → Nix migration
   - Consolidated package management in `modules/home-manager/packages.nix`
   - Historical Brewfile preserved for reference: `Brewfile`, `BREWFILE_MIGRATION.md`

4. **Secrets Management**: Implemented sops-nix with age encryption
   - API keys: Anthropic, GitHub (personal + Fortium)
   - Secrets exposed via .envrc for direnv integration

5. **Browser Configuration**: Added Qutebrowser with Catppuccin theme
   - Full TRD/PRD documentation in `docs/`

## Context Management for Claude

### Agent Mesh Integration

**Recommended Agents for This Repository**:

1. **Infrastructure Development** (`ensemble-infrastructure:infrastructure-developer`)
   - Nix configuration modifications
   - System-level darwin changes
   - Package overlay development

2. **Documentation** (`ensemble-development:documentation-specialist`)
   - Update TRD/PRD documents
   - Maintain migration guides (BREWFILE_MIGRATION.md, CHEZMOI_MIGRATION.md)

3. **Git Workflow** (`ensemble-git:git-workflow`)
   - Conventional commits for nix configuration changes
   - Use git-town for feature branches

### Project-Specific Patterns

**When modifying configurations**:

1. Read the existing module file first (never assume structure)
2. Preserve existing patterns (e.g., commented packages)
3. Test with `just deploy-nc` to avoid cache issues
4. Document breaking changes in commit messages

**Secrets workflow**:

- Never commit plaintext secrets
- Use `./edit_secrets.sh` to modify encrypted secrets
- Reference secrets via sops paths: `/run/secrets/<key>`

**Package additions**:

- Check nixpkgs availability before adding
- Document unavailable packages (e.g., "# aider - not in nixpkgs, available via brew")
- Use overlays for custom builds (see `overlays/sketchybar-lua/`)

### Memory Management

**High-priority context to retain**:

- Current flake inputs and versions (see `flake.lock`)
- Active launchd services (kanata, karabiner - see `modules/darwin/services.nix`)
- tmux configuration with Ctrl+Space prefix (see `modules/home-manager/tmux/`)
- Secrets management via sops-nix (never suggest plaintext API keys)

**Low-priority context** (reference as needed):

- Historical Brewfile entries (already migrated)
- Commented-out configurations (kept for reference)
- Deprecated modules (spacemacs.nix, spacevim.nix)

## Output Preferences

### Code Style

- Nix: 2-space indentation, attribute sets aligned
- Comments: Inline for context, block for major sections
- Imports: Group by category (system, user, programs)

### Communication Style

- Be concise and technical
- Reference file paths with line numbers (e.g., `modules/home-manager/packages.nix:18`)
- Use code blocks for commands and configurations
- Explain "why" for non-obvious changes (especially macOS defaults)

### Workflow Preferences

- Use `just` commands over raw `darwin-rebuild`
- Test changes with `just deploy-nc` when modifying flake inputs
- Suggest `just debug` for troubleshooting
- Always check `just history` before major rollbacks

## Common Tasks

### Adding a New Package

```nix
# Add to modules/home-manager/packages.nix
home.packages = with pkgs; [
  # ... existing packages
  new-package-name
];
```

Then: `just deploy`

### Modifying System Defaults

```nix
# Edit modules/darwin/system.nix
system.defaults.NSGlobalDomain = {
  AppleShowAllExtensions = true;
  # ... other settings
};
```

Then: `just deploy`

### Adding a Secret

```bash
./edit_secrets.sh
# Add key-value pairs in YAML format
# Save and exit

# Reference in configuration:
export NEW_SECRET="$(cat /run/secrets/new_key 2>/dev/null || echo "")"
```

Then: `just deploy`

### Updating Dependencies

```bash
# Update all inputs
just up

# Update specific input (e.g., home-manager)
just upp i=home-manager

# Deploy changes
just deploy
```

## Troubleshooting

### Build Failures

```bash
# Clear cache and rebuild
just deploy-nc

# Verbose output for debugging
just debug

# Check build logs
nix log /nix/store/<hash>-darwin-system-<version>
```

### Rollback

```bash
# List generations
just history

# Rollback to previous generation
sudo nix-env --rollback --profile /nix/var/nix/profiles/system
sudo /nix/var/nix/profiles/system/activate

# Or specify generation number
sudo nix-env --switch-generation <n> --profile /nix/var/nix/profiles/system
```

### Clean Up Space

```bash
# Remove generations older than 7 days
just clean

# Garbage collect unused packages (>14 days)
just gc

# Combined cleanup
just real-clean
```

## Claude Code Integration

### IDE Configuration

**Emacs Integration**:

- Connected via `claude-code-ide.el`
- LSP via xref (find references, definitions)
- Tree-sitter syntax analysis
- Imenu symbol navigation
- Flycheck/Flymake diagnostics

**Coordinate System**:

- Lines: 1-based (line 1 = first line)
- Columns: 0-based (column 0 = first character)
- Example: First character in file is at line 1, column 0

### Available MCP Servers

1. **Claude in Chrome**: Browser automation for documentation lookup
2. **Emacs Tools**: Direct IDE integration for navigation and analysis

### Suggested Workflows

**Exploring package availability**:

```bash
# Use nix-search-tv (installed)
nix-search-tv <package-name>

# Or search nixpkgs
nix search nixpkgs <package-name>
```

**Validating Nix expressions**:

```bash
# Check flake
nix flake check

# Evaluate configuration
nix eval .#darwinConfigurations."Leos-MacBook-Pro".config.system.build.toplevel
```

**Finding configuration options**:

- home-manager: <https://nix-community.github.io/home-manager/options.xhtml>
- nix-darwin: <https://daiderd.com/nix-darwin/manual/index.html>

## Related Documentation

- `BREWFILE_MIGRATION.md`: Historical package migration from Homebrew
- `CHEZMOI_MIGRATION.md`: Dotfiles migration from chezmoi
- tmux layout scripts: `modules/home-manager/tmux/layouts/` (session startup scripts)
- `docs/PRD/`: Product requirements documents
- `docs/TRD/`: Technical requirements documents
- `Justfile`: All available commands and recipes

## Notes

- **Nix Learning Resources**: <https://zero-to-nix.com/>, <https://nix.dev/>
- **Darwin Options**: <https://daiderd.com/nix-darwin/manual/index.html>
- **Home Manager**: <https://nix-community.github.io/home-manager/>
- **Flakes Reference**: <https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html>

---

Last Updated: 2026-02-16
Configuration Version: 25.11.3bda9f6

<!-- br-agent-instructions-v1 -->

---

## Beads Workflow Integration

This project uses [beads_rust](https://github.com/Dicklesworthstone/beads_rust) (`br`/`bd`) for issue tracking. Issues are stored in `.beads/` and tracked in git.

### Essential Commands

```bash
# View ready issues (unblocked, not deferred)
br ready              # or: bd ready

# List and search
br list --status=open # All open issues
br show <id>          # Full issue details with dependencies
br search "keyword"   # Full-text search

# Create and update
br create --title="..." --description="..." --type=task --priority=2
br update <id> --status=in_progress
br close <id> --reason="Completed"
br close <id1> <id2>  # Close multiple issues at once

# Sync with git
br sync --flush-only  # Export DB to JSONL
br sync --status      # Check sync status
```

### Workflow Pattern

1. **Start**: Run `br ready` to find actionable work
2. **Claim**: Use `br update <id> --status=in_progress`
3. **Work**: Implement the task
4. **Complete**: Use `br close <id>`
5. **Sync**: Always run `br sync --flush-only` at session end

### Key Concepts

- **Dependencies**: Issues can block other issues. `br ready` shows only unblocked work.
- **Priority**: P0=critical, P1=high, P2=medium, P3=low, P4=backlog (use numbers 0-4, not words)
- **Types**: task, bug, feature, epic, chore, docs, question
- **Blocking**: `br dep add <issue> <depends-on>` to add dependencies

### Session Protocol

**Before ending any session, run this checklist:**

```bash
git status              # Check what changed
git add <files>         # Stage code changes
br sync --flush-only    # Export beads changes to JSONL
git commit -m "..."     # Commit everything
git push                # Push to remote
```

### Best Practices

- Check `br ready` at session start to find available work
- Update status as you work (in_progress → closed)
- Create new issues with `br create` when you discover tasks
- Use descriptive titles and set appropriate priority/type
- Always sync before ending session

<!-- end-br-agent-instructions -->
