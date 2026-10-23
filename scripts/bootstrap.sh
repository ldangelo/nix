#!/usr/bin/env bash
# Bootstrap a fresh Mac: Homebrew -> chezmoi + 1Password CLI -> chezmoi init/apply.
# Usage: git clone git@github.com:ldangelo/nix.git ~/nix && ~/nix/scripts/bootstrap.sh
set -euo pipefail
REPO="$(cd "$(dirname "$0")/.." && pwd)"

if ! command -v brew >/dev/null 2>&1 && [ ! -x /opt/homebrew/bin/brew ]; then
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi
eval "$(/opt/homebrew/bin/brew shellenv)"
brew install chezmoi
brew install --cask 1password-cli || true

# .chezmoiroot in the repo points chezmoi at home/
chezmoi init --source "$REPO" --apply
echo "Done. Open a new shell. For secrets see docs/CHEZMOI.md#secrets-1password"
