# just is a command runner, Justfile is very similar to Makefile, but simpler.

# Use a non-login shell. macOS Terminal's /etc/bashrc_Apple_Terminal
# emits "Saving/Restored session" lines on login shells, which
# contaminate backtick captures like `uname -s` below.
set shell := ["bash", "-c"]

# Detect operating system
os := `uname -s`

# Load single-user Nix if needed, then run nix.
nix := 'if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then source "$HOME/.nix-profile/etc/profile.d/nix.sh"; fi; nix'

# On macOS this is where nix-darwin installs darwin-rebuild;
# /run/current-system is not guaranteed to exist.
darwin_rebuild := '/nix/var/nix/profiles/system/sw/bin/darwin-rebuild'

# Linux recipes
# NIX_CONFIG makes fallback inherited by home-manager's nested nix build.
deploy-nc-linux:
  export NIX_CONFIG="fallback = true"; {{nix}} run github:nix-community/home-manager -- -b bak switch --flake .#ldangelo-linux

deploy-rebuild-linux:
  export NIX_CONFIG="fallback = true"; {{nix}} run github:nix-community/home-manager -- -b bak switch --flake .#ldangelo-linux

deploy-linux:
  export NIX_CONFIG="fallback = true"; {{nix}} run github:nix-community/home-manager -- -b bak switch --flake .#ldangelo-linux

debug-linux:
  export NIX_CONFIG="fallback = true"; {{nix}} run github:nix-community/home-manager -- -b bak switch --flake .#ldangelo-linux --show-trace

hm-switch-linux:
  export NIX_CONFIG="fallback = true"; {{nix}} run github:nix-community/home-manager -- -b bak switch --flake .#ldangelo-linux

hm-build-linux:
  export NIX_CONFIG="fallback = true"; {{nix}} run github:nix-community/home-manager -- build --flake .#ldangelo-linux

# macOS recipes
# Use GUI askpass when running locally; fall back to TTY prompt over SSH or in CI
_sudo_prefix := if env_var_or_default("SSH_CLIENT", "") + env_var_or_default("SSH_TTY", "") + env_var_or_default("SSH_CONNECTION", "") != "" { "sudo" } else { "SUDO_ASKPASS=" + env_var_or_default("HOME", "~") + "/.local/bin/sudo-askpass sudo -AH" }

deploy-nc-macos:
  host="$(scutil --get LocalHostName)"; {{_sudo_prefix}} {{darwin_rebuild}} switch --flake .#$host --option eval-cache false

deploy-rebuild-macos:
  host="$(scutil --get LocalHostName)"; {{_sudo_prefix}} {{darwin_rebuild}} switch --rebuild --flake .#$host

deploy-macos:
  host="$(scutil --get LocalHostName)"; {{_sudo_prefix}} {{darwin_rebuild}} switch --flake .#$host

debug-macos:
  host="$(scutil --get LocalHostName)"; {{_sudo_prefix}} {{darwin_rebuild}} switch --flake .#$host --show-trace --verbose

hm-switch-macos:
  {{nix}} run github:nix-community/home-manager -- -b bak switch --flake .#ldangelo

hm-build-macos:
  {{nix}} run github:nix-community/home-manager -- build --flake .#ldangelo

# Default recipes that detect OS and run appropriate command
deploy-nc:
  if [ "{{os}}" = "Darwin" ]; then just deploy-nc-macos; else just deploy-nc-linux; fi

deploy-rebuild:
  if [ "{{os}}" = "Darwin" ]; then just deploy-rebuild-macos; else just deploy-rebuild-linux; fi

deploy:
  if [ "{{os}}" = "Darwin" ]; then just deploy-macos; else just deploy-linux; fi

debug:
  if [ "{{os}}" = "Darwin" ]; then just debug-macos; else just debug-linux; fi

hm-switch:
  if [ "{{os}}" = "Darwin" ]; then just hm-switch-macos; else just hm-switch-linux; fi

hm-build:
  if [ "{{os}}" = "Darwin" ]; then just hm-build-macos; else just hm-build-linux; fi

up:
  {{nix}} flake update

# Update specific input
# usage: just upp home-manager
# also tolerates: just upp i=home-manager
upp input:
  input="{{input}}"; input="${input#i=}"; {{nix}} flake update "$input"

# GC and cleanup
gc:
  {{nix}}-collect-garbage --delete-older-than 3d

real-clean: gc
  {{nix}} profile wipe-history --profile /nix/var/nix/profiles/system --older-than 3d

repl:
  {{nix}} repl -f flake:nixpkgs

# Initialize home-manager on a new machine
hm-init:
  {{nix}} run github:nix-community/home-manager -- init --switch
