---
disclose: always
summary: Linux-specific flake and home-manager configuration separate from macOS nix-darwin setup
triggers: [linux, nixos, nixos-rebuild, home-manager, flake]
---

# Linux Flake

Linux-specific nix-darwin equivalent configuration. Separate from darwin flake.

## Structure

| File | Purpose |
|------|---------|
| `flake.nix` | Linux flake entry point |
| `flake.lock` | Locked inputs |
| `home-manager/` | Linux-specific home-manager config |
| `README.md` | Setup instructions |

## Notes

- Not used on macOS. Linux config is in `modules/linux/` as a separate flake.
- If working on Linux, deploy with `nixos-rebuild` instead of `darwin-rebuild`.
- Shared modules in `modules/home-manager/` can be reused.
