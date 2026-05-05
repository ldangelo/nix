# just is a command runner, Justfile is very similar to Makefile, but simpler.
#
############################################################################
############################################################################
#
#  Nix commands related to the local machine
#
############################################################################

# Detect operating system
os := `uname -s`

# Linux recipes
deploy-nc-linux:
  nix run github:nix-community/home-manager -- switch --flake .#ldangelo-linux

deploy-rebuild-linux:
  nix run github:nix-community/home-manager -- switch --flake .#ldangelo-linux

deploy-linux:
  nix run github:nix-community/home-manager -- switch --flake .#ldangelo-linux

debug-linux:
  nix run github:nix-community/home-manager -- switch --flake .#ldangelo-linux --show-trace

hm-switch-linux:
  nix run github:nix-community/home-manager -- switch --flake .#ldangelo-linux

hm-build-linux:
  nix run github:nix-community/home-manager -- build --flake .#ldangelo-linux

# macOS recipes
deploy-nc-macos:
  SUDO_ASKPASS=~/.local/bin/sudo-askpass sudo -AH darwin-rebuild switch --flake .#{{host}} --option eval-cache false

deploy-rebuild-macos:
  SUDO_ASKPASS=~/.local/bin/sudo-askpass sudo -AH darwin-rebuild switch --rebuild --flake .#{{host}}

deploy-macos:
  SUDO_ASKPASS=~/.local/bin/sudo-askpass sudo -AH darwin-rebuild switch --flake .#{{host}}

debug-macos:
  SUDO_ASKPASS=~/.local/bin/sudo-askpass sudo -AH darwin-rebuild switch --flake .#{{host}} --show-trace --verbose

hm-switch-macos:
  nix run github:nix-community/home-manager -- switch --flake .#ldangelo

hm-build-macos:
  nix run github:nix-community/home-manager -- build --flake .#ldangelo

# Default recipes that detect OS and run appropriate command
host := `hostname`

deploy-nc:
  if [ "$(os)" = "Darwin" ]; then just deploy-nc-macos; else just deploy-nc-linux; fi

deploy-rebuild:
  if [ "$(os)" = "Darwin" ]; then just deploy-rebuild-macos; else just deploy-rebuild-linux; fi

deploy:
  if [ "$(os)" = "Darwin" ]; then just deploy-macos; else just deploy-linux; fi

debug:
  if [ "$(os)" = "Darwin" ]; then just debug-macos; else just debug-linux; fi

hm-switch:
  if [ "$(os)" = "Darwin" ]; then just hm-switch-macos; else just hm-switch-linux; fi

hm-build:
  if [ "$(os)" = "Darwin" ]; then just hm-build-macos; else just hm-build-linux; fi

up:
  nix flake update

# Update specific input
# usage: just upp i=home-manager
upp i="":
  nix flake lock --update-input {{i}}

# GC and cleanup
gc:
  nix-collect-garbage --delete-older-than 3d

real-clean: gc
  # remove all generations older than 7 days
  nix profile wipe-history --profile /nix/var/nix/profiles/system --older-than 3d

repl:
  nix repl -f flake:nixpkgs

# Initialize home-manager on a new machine
hm-init:
  if [ "$(os)" = "Darwin" ]; then just hm-init-macos; else just hm-init-linux; fi

hm-init-macos:
  nix run github:nix-community/home-manager -- init --switch

hm-init-linux:
  nix run github:nix-community/home-manager -- init --switch
