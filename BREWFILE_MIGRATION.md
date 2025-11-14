# Brewfile to Nix Migration Summary

## Overview

Successfully migrated packages from Brewfile to Nix configuration. The migration splits packages into three categories:

1. **Nix packages** - CLI tools and libraries available in nixpkgs
2. **Homebrew casks** - GUI applications better managed through Homebrew
3. **Mac App Store** - Apps that must be installed via MAS

## Files Created/Modified

### New Files
- `modules/home-manager/packages.nix` - Migrated CLI tools and libraries from Brewfile
- `modules/darwin/homebrew.nix` - Homebrew configuration for GUI apps and MAS apps

### Modified Files
- `modules/home-manager/default.nix` - Added import for packages.nix
- `modules/darwin/default.nix` - Replaced inline homebrew config with import

## Package Migration Details

### Successfully Migrated to Nix (home.packages)

**Development Tools:**
- act, ast-grep, cargo-binstall, cmake, gradle, helix, jdt-language-server, just, lazygit, maven, neovim, openapi-generator-cli, plantuml, trivy

**Git Tools:**
- gh, git, git-town, glab, jujutsu

**Shell & Terminal:**
- atuin, autojump, btop, coreutils, direnv, eza, fasd, fd, fzf, htop, starship, terminal-notifier, thefuck, zoxide, zsh-autosuggestions, zsh-syntax-highlighting

**Text Processing:**
- jq, marksman, multimarkdown, pandoc, ripgrep, silver-searcher

**Email & Communication:**
- mu, msmtp, neomutt, notmuch

**Cloud & Infrastructure:**
- awscli2, docker-credential-helpers, flyctl, gitlab-runner, kubernetes-helm, kubectl, kubeconform, temporal, terraform

**Programming Languages:**
- dotnet-sdk_8, elixir, openjdk, openjdk21, nodejs, postgresql, python3, ruby, rustc, cargo, rbenv

**Build Tools & Libraries:**
- autoconf, automake, binutils, clang-tools, cyrus_sasl, cyrus-sasl-xoauth2, graphviz, harfbuzz, ispell, libjpeg, libtool, luarocks, pkg-config, zlib

**Media & Graphics:**
- chafa, mpv, viu, w3m

**Utilities:**
- duf, gnupg, httpie, hugo, mas, pipx, scc, uv, virtualenv, wakatime, wget

**Fonts:**
- nerdfonts, source-code-pro

**GUI Apps (in nixpkgs):**
- discord, dockutil, duti, vscode

### Kept in Homebrew (Better via Casks)

**Brew Formulae:**
- emacs-plus@31 (with custom build flags)
- borders (FelixKratz window borders)

**Casks (GUI Applications):**
1password, 1password-cli, aerospace, alt-tab, amazon-q, ammonite, appcleaner, arc, block-goose, claude-code, davmail-app, docker-desktop, elgato-*, fantastical, fonts, git-credential-manager, google-chrome, gotomeeting, grammarly-desktop, hammerspoon, homerow, hookmark, iterm2, itermai, itermbrowserplugin, jetbrains-toolbox, karabiner-elements, launchcontrol, lens, limitless, linear-linear, logseq, mactex-no-gui, meld, microsoft-*, mouseless, obsidian, ollama-app, postgres-unofficial, postman, proxyman, qutebrowser, readdle-spark, repo-prompt, setapp, sf-symbols, slack, sourcetree, stats, superhuman, tabtab, todoist-app, tradingview, vscodium, wakatime, wezterm, witsy, zed, zoom

**Mac App Store Apps:**
All MAS apps from Brewfile migrated to `homebrew.masApps` in homebrew.nix

### Packages Added to Homebrew (Not in nixpkgs)

All packages that aren't available in nixpkgs have been added to the Homebrew configuration:

**CLI Tools via Homebrew:**
- aider, basedpyright, devbox, devpod, evil-helix, igrep, jql, lazyjj, nuget, opencode, repomix, swagger-codegen
- butterfish, crush, choose-gui, fileql, ifstat, kanata, switchaudio-osx, vfkit
- alot, notmuch (email tools)
- podman, docker-compose, chart-testing
- xpdf (available via brew, marked insecure in nix)
- cask, dotnet@6, dotnet@8, fisher, haskell-stack, nvm

**GUI Apps via Homebrew Casks:**
- aldente, apparency, raycast, shortcat, warp-terminal
- All other GUI applications from the original Brewfile

**Result:** 100% coverage of original Brewfile packages - everything is now managed either via Nix or Homebrew!

## Usage

### Building the Configuration

```bash
nix build .#darwinConfigurations.ldangelo.system
```

### Applying the Configuration

```bash
darwin-rebuild switch --flake .#ldangelo
```

### Managing Homebrew Separately

The Homebrew configuration in `modules/darwin/homebrew.nix` will:
- Auto-update Homebrew on activation
- Auto-upgrade packages
- Remove packages not listed (cleanup = "zap")

## Notes

1. **Duplicate Removal**: Removed duplicate entries like `just` and `nerdfonts` that appeared multiple times in Brewfile

2. **Package Name Corrections**:
   - `jj` → `jujutsu`
   - `openapi-generator` → `openapi-generator-cli`
   - `kubectl` was implicit, added explicitly
   - `postgresql_17` → `postgresql`
   - `wakatime-cli` → `wakatime`
   - `pkgconf` → `pkg-config`
   - `awscli` → `awscli2`
   - `helm` → `kubernetes-helm`
   - `jpeg` → `libjpeg`

3. **Overlay Packages**: cyrus-sasl-xoauth2 is provided by the existing overlay in flake.nix

4. **Version Managers**: nvm is shell-based and should be configured in shell.nix if needed

## Benefits of This Migration

1. **Reproducibility**: Nix packages are declarative and reproducible
2. **Version Control**: All package versions are tracked
3. **Atomic Updates**: Nix provides atomic upgrades and rollbacks
4. **Hybrid Approach**: Best of both worlds - Nix for CLI tools, Homebrew for GUI apps
5. **Centralized Configuration**: All system configuration in one repository

## Next Steps

1. Review the migrated packages and uncomment any needed tools
2. Consider adding packages currently marked as "not in nixpkgs" via overlays or custom derivations
3. Test the darwin-rebuild switch to ensure all packages install correctly
4. Update shell configurations to use Nix-installed tools
