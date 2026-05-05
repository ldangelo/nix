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

host := `hostname`
# For macOS, use LocalHostName
if $(os) == "Darwin", host := `scutil --get LocalHostName`

deploy-nc:
  if $(os) == "Darwin" {
    SUDO_ASKPASS=~/.local/bin/sudo-askpass sudo -AH darwin-rebuild switch --flake .#{{host}} --option eval-cache false
  } else {
    nix run github:nix-community/home-manager -- switch --flake .#ldangelo-linux
  }

deploy-rebuild:
  if $(os) == "Darwin" {
    SUDO_ASKPASS=~/.local/bin/sudo-askpass sudo -AH darwin-rebuild switch --rebuild --flake .#{{host}}
  } else {
    nix run github:nix-community/home-manager -- switch --flake .#ldangelo-linux
  }

deploy:
  if $(os) == "Darwin" {
    SUDO_ASKPASS=~/.local/bin/sudo-askpass sudo -AH darwin-rebuild switch --flake .#{{host}}
  } else {
    nix run github:nix-community/home-manager -- switch --flake .#ldangelo-linux
  }

debug:
  if $(os) == "Darwin" {
    SUDO_ASKPASS=~/.local/bin/sudo-askpass sudo -AH darwin-rebuild switch --flake .#{{host}} --show-trace --verbose
  } else {
    nix run github:nix-community/home-manager -- switch --flake .#ldangelo-linux --show-trace
  }

# Home-manager specific commands for all systems
hm-switch:
  if $(os) == "Darwin" {
    nix run github:nix-community/home-manager -- switch --flake .#ldangelo
  } else {
    nix run github:nix-community/home-manager -- switch --flake .#ldangelo-linux
  }

hm-build:
  if $(os) == "Darwin" {
    nix run github:nix-community/home-manager -- build --flake .#ldangelo
  } else {
    nix run github:nix-community/home-manager -- build --flake .#ldangelo-linux
  }

up:
  nix flake update

# Update specific input
# usage: just upp i=home-manager
upp:
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
  if $(os) == "Darwin" {
    nix run github:nix-community/home-manager -- init --switch
  } else {
    nix run github:nix-community/home-manager -- init --switch
  }
