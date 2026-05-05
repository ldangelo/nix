# just is a command runner, Justfile is very similar to Makefile, but simpler.

set shell := ["bash", "-lc"]

# Detect operating system
os := `uname -s`

# Load single-user Nix if needed, then run nix.
nix := 'if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then source "$HOME/.nix-profile/etc/profile.d/nix.sh"; fi; nix'

# Linux recipes
deploy-nc-linux:
  {{nix}} run github:nix-community/home-manager -- -b bak switch --flake .#ldangelo-linux

deploy-rebuild-linux:
  {{nix}} run github:nix-community/home-manager -- -b bak switch --flake .#ldangelo-linux

deploy-linux:
  {{nix}} run github:nix-community/home-manager -- -b bak switch --flake .#ldangelo-linux

debug-linux:
  {{nix}} run github:nix-community/home-manager -- -b bak switch --flake .#ldangelo-linux --show-trace

hm-switch-linux:
  {{nix}} run github:nix-community/home-manager -- -b bak switch --flake .#ldangelo-linux

hm-build-linux:
  {{nix}} run github:nix-community/home-manager -- build --flake .#ldangelo-linux

# macOS recipes
deploy-nc-macos:
  host="$(scutil --get LocalHostName)"; SUDO_ASKPASS=~/.local/bin/sudo-askpass sudo -AH darwin-rebuild switch --flake .#$host --option eval-cache false

deploy-rebuild-macos:
  host="$(scutil --get LocalHostName)"; SUDO_ASKPASS=~/.local/bin/sudo-askpass sudo -AH darwin-rebuild switch --rebuild --flake .#$host

deploy-macos:
  host="$(scutil --get LocalHostName)"; SUDO_ASKPASS=~/.local/bin/sudo-askpass sudo -AH darwin-rebuild switch --flake .#$host

debug-macos:
  host="$(scutil --get LocalHostName)"; SUDO_ASKPASS=~/.local/bin/sudo-askpass sudo -AH darwin-rebuild switch --flake .#$host --show-trace --verbose

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
