# Darwin Module AGENTS.md

System-level macOS configuration. Loaded by nix-darwin at build time.

## Structure

| File | Purpose |
|------|---------|
| `system.nix` | macOS defaults: dock, finder, keyboard, menu bar, system preferences |
| `homebrew.nix` | Homebrew formulae + casks. Add packages here. |
| `services.nix` | launchd services: kanata, karabiner, tailscale |
| `sops.nix` | Sops-nix setup for encrypted secrets |

## Conventions

- Package additions go in `homebrew.nix` under `brews` or `casks`
- New macOS defaults go in `system.nix`
- Launchd services go in `services.nix` (use `launchd.services` or `launchd.agents`)
- Secrets in `sops.nix` (use `./edit_secrets.sh` to add new ones)

## Adding a Homebrew Package

1. Find the correct tap (check `brew search <name>`)
2. Add to `brews` list or `casks` list in `homebrew.nix`
3. Run `just deploy-nc` to rebuild
