#!/usr/bin/env bash
# Script to edit encrypted secrets with sops

export SOPS_AGE_KEY_FILE=~/.config/sops/age/keys.txt
nix-shell -p sops --run "sops secrets/secrets.yaml"
