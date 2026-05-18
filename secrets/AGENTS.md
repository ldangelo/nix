# Secrets Directory AGENTS.md

Sops-nix encrypted secrets. Files here are encrypted at rest.

## Structure

| File | Purpose |
|------|---------|
| `*.yaml` | Encrypted secret files (SSH keys, tokens, passwords) |

## Adding Secrets

1. Run `./edit_secrets.sh` — opens encrypted file in editor
2. Add key-value pairs under the appropriate section
3. Save and exit. `sops` auto-encrypts on save.

## Managing Sops Keys

Age key is stored in `.ssh/id_ed25519` (imported by sops-nix on deploy).
Add new users/decryptors by editing `modules/darwin/sops.nix`.

## Accessing Secrets

After deploy, secrets are available at paths specified in `modules/darwin/sops.nix` filePolicies.
