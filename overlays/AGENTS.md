---
disclose: always
summary: Custom nixpkgs overlay structure, naming conventions, and adding overlay packages
triggers: [overlays, overlay, nixpkgs, package, sketchybar]
---

# Package Overlays

Custom nixpkgs package overlays — modify or extend the default package set.

## Structure

| File | Purpose |
|------|---------|
| `sketchybar-lua/` | Sketchybar Lua bindings override |

## How Overlays Work

Each directory in `overlays/` exports a function `(self: super) -> packages` that gets merged into the nixpkgs package set.

## Adding an Overlay

1. Create `overlays/<name>/default.nix`
2. Export a function returning modified/added packages
3. Reference from `flake.nix` or `darwinOverlay`

## Conventions

- Overlay names must match directory names
- Package names should use `<overlay>-<pkg>` prefix to avoid conflicts
- Keep overlays minimal — prefer adding to nixpkgs upstream when possible
