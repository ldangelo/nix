# Dotfiles are managed by chezmoi (source: ./home via .chezmoiroot); packages by Homebrew.
# See docs/CHEZMOI.md.

set shell := ["bash", "-c"]

# Show available recipes
default:
  @just --list

# Apply dotfiles + run chezmoi scripts (brew bundle, tmux plugins, launchd, …)
apply:
  chezmoi apply --verbose

# Preview what `apply` would change
diff:
  chezmoi diff

# Preview, then apply with prompts for conflicts
apply-interactive:
  chezmoi diff; chezmoi apply --verbose --interactive

# Pull live edits of a managed file back into the source (e.g. just re-add ~/.config/gh/config.yml)
re-add *files:
  chezmoi re-add {{files}}

# Start managing a file
add *files:
  chezmoi add {{files}}

# Install everything in the Brewfile (no upgrades)
brew:
  chezmoi execute-template < home/dot_Brewfile.tmpl | brew bundle --file=- --no-upgrade

# Upgrade brew packages and refresh externals (oh-my-zsh, TPM)
upgrade:
  brew update && brew upgrade
  chezmoi apply --refresh-externals

# Check Brewfile drift: installed packages that aren't in the Brewfile
brew-drift:
  chezmoi execute-template < home/dot_Brewfile.tmpl | brew bundle cleanup --file=-

# Sanity checks: no leftover nix references in the source
check:
  @! grep -rIn -E '/nix/store|nix-profile|/run/secrets|sops-nix|hm-session' home || (echo "nix references found" && exit 1)
  @chezmoi verify >/dev/null 2>&1 || echo "note: live files differ from source (run 'just diff')"
