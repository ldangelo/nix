# just is a command runner, Justfile is very similar to Makefile, but simpler.

############################################################################
#
#  Nix commands related to the local machine
#
############################################################################

deploy-nc:
  darwin-rebuild switch --flake .#ldangelo --option eval-cache false

deploy-rebuild:
  darwin-rebuild switch --rebuild --flake .#ldangelo

deploy:
  darwin-rebuild switch --flake .#ldangelo

debug:
  darwin-rebuild switch --flake .#ldangelo --show-trace --verbose

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
