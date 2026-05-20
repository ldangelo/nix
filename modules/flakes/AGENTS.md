---
disclose: always
summary: Nix flake input management, adding/updating inputs, and overlay references
triggers: [flake, flakes, inputs, nixpkgs, update, overlays, just up]
---

# Flake Inputs

Nix flake inputs — external dependencies pinned and versioned.

## How It Works

`flake.nix` at the root declares inputs via `nix-darwin` / `flake-parts`. Each input is pinned to a specific commit/tag for reproducibility.

## Adding a New Input

1. Open `flake.nix` in the repository root
2. Add to the `inputs` section with a git URL or nixpkgs channel
3. Use it in the appropriate module via `pkgs.importPath` or `self.inputs.x`

## Updating Inputs

Run `just up` to pull latest from all inputs. Review diffs before deploying.

## Overlays

Custom nixpkgs overrides live in `overlays/`. Each overlay is a function that takes `(self: super)` and returns modified package set.

See [overlays/AGENTS.md](overlays/AGENTS.md) for overlay conventions.
