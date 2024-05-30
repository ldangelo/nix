# just is a command runner, Justfile is very similar to Makefile, but simpler.

############################################################################
#
#  Nix commands related to the local machine
#
############################################################################
flakename := '.'

default:
  @just --list

deploy-nc:
  darwin-rebuild switch --flake {{flakename}} --option eval-cache false

build:
  darwin-rebuild build --flake {{flakename}}

build-nc:
  darwin-rebuild build --flake {{flakename}} --option eval-cache false --show-trace

deploy:
  darwin-rebuild switch --flake {{flakename}}

debug:
  darwin-rebuild switch --flake {{flakename}} --show-trace --verbose

up:
  nix flake update

# Update specific input
# usage: make upp i=home-manager
upp:
  nix flake lock --update-input $(i)

history:
  nix profile history --profile /nix/var/nix/profiles/system

repl:
  nix repl -f flake:nixpkgs

clean: up
  # remove all generations older than 7 days
  sudo nix profile wipe-history --profile /nix/var/nix/profiles/system  --older-than 7d

gc:
  # garbage collect all unused nix store entries
  nix-collect-garbage --delete-older-than 14d

real-clean: clean gc
