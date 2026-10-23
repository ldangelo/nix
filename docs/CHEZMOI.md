# Dotfiles & machine setup: chezmoi + Homebrew

This repo replaced Nix (nix-darwin + home-manager) with:

- **chezmoi** — dotfiles as plain, writable files (`home/` is the chezmoi source)
- **Homebrew** — packages and apps, declared in `home/dot_Brewfile.tmpl`
- **1Password CLI** — secrets, rendered by chezmoi templates

Why: store symlinks broke tools that write their own config (pi/omp, gh, jj, git, …), and
the extra layer made it hard for agents to edit the system. Everything under `~` is now a
real file; there is no build/deploy step.

## Layout

```
.chezmoiroot                  -> "home"  (chezmoi source root)
home/
  dot_zshrc, dot_zshenv, …    -> ~/.zshrc …            (dot_ = leading ".")
  dot_config/…                -> ~/.config/…
  private_*                   -> mode 0600/0700
  executable_*                -> +x
  create_*                    -> written once, never overwritten (files programs rewrite: gh, worktrunk)
  *.tmpl                      -> Go templates (machine data: .workstation, .secrets, .vault)
  dot_Brewfile.tmpl           -> ~/.Brewfile
  .chezmoiexternal.toml       -> git checkouts: oh-my-zsh, TPM, tmux-sensible
  .chezmoiscripts/            -> brew bundle, tmux plugins, rift plugins (cargo), macOS defaults, launchd
  Library/LaunchAgents/       -> pizauth + rift plugin agents
scripts/                      -> bootstrap.sh, migrate-secrets-to-1password.sh
```

## Daily workflow

| Task | Command |
| --- | --- |
| Apply everything | `just apply` (= `chezmoi apply`) |
| See what would change | `just diff` |
| Edit a managed file | `chezmoi edit ~/.zshrc` (or edit under `home/`, then `just apply`) |
| A tool rewrote its config, keep it | `chezmoi re-add ~/.config/foo` |
| Start managing a file | `chezmoi add ~/.config/foo` |
| Add a package | edit `home/dot_Brewfile.tmpl`, `just apply` (runs `brew bundle`) |
| Update packages | `just upgrade` |

**Agents:** edit files under `home/` (or the live file, then `chezmoi re-add`), then commit.
There is nothing to build. Never edit generated output under `/nix/store` (gone).

Do not manage `~/.pi` or `~/.omp`: pi/omp own those directories (settings, auth, sessions).

## New machine

```bash
git clone git@github.com:ldangelo/nix.git ~/nix && ~/nix/scripts/bootstrap.sh
```

It installs Homebrew, chezmoi and the 1Password CLI, then `chezmoi init --apply`
(prompting once for `workstation`, `secrets`, `vault`).

## Secrets (1Password)

Secrets are never in this repo. Templates read `op://<vault>/<item>/credential`, where the item
name is the old sops key (`anthropic_api_key`, `mac_mail_key`, …).

1. One time, while sops still works: `VAULT=Private scripts/migrate-secrets-to-1password.sh --dry-run`, then without `--dry-run`.
2. Sign in: `op signin` (or enable the 1Password desktop-app CLI integration).
3. Set `secrets = true` in `~/.config/chezmoi/chezmoi.toml` and `chezmoi apply`.

This renders `~/.config/secrets/env` (sourced by `~/.envrc`), `~/.config/secrets/mac_mail_key`
(msmtp/mbsync) and `~/.wakatime.cfg`, all mode 0600.

## What replaced what

| Nix | Now |
| --- | --- |
| `home.packages`, `homebrew.*` | `~/.Brewfile` |
| `programs.*` modules | plain config files in `home/dot_config/…` |
| tmux plugins (nix-built) | TPM (`@plugin` lines in tmux.conf) |
| oh-my-zsh / zsh plugins | `~/.oh-my-zsh` external + brew zsh plugins |
| `system.defaults`, caps→esc | `.chezmoiscripts/…macos-defaults` |
| launchd (pizauth, rift plugins, borders) | `Library/LaunchAgents/*.plist` + `brew services` |
| rift plugins, pizauth | `cargo install` (pinned revs) |
| Touch ID sudo | printed one-liner (needs root) |
| sops-nix | 1Password CLI |
| catppuccin theming | static theme files under `dot_config/catppuccin/` |
