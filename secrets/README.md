# Secrets Management with sops-nix

This directory contains encrypted secrets managed by sops-nix.

## Setup Complete

Your sops-nix configuration is ready to use!

### Encryption Keys

- **User age key**: `~/.config/sops/age/keys.txt`
- **SSH key**: `~/.ssh/id_ed25519` (also converted to age format)

Both keys can decrypt the secrets in this repository.

## Creating/Editing Secrets

To create or edit the secrets file:

```bash
nix-shell -p sops --run "sops secrets/secrets.yaml"
```

This will open your default editor (set via `$EDITOR` or defaults to vim) with the decrypted secrets. When you save and exit, sops will automatically encrypt the file.

## Accessing Secrets

After running `darwin-rebuild switch`, secrets will be available at:

- `/run/secrets/github/token` - GitHub token

## Adding New Secrets

1. Edit the secrets file:
   ```bash
   nix-shell -p sops --run "sops secrets/secrets.yaml"
   ```

2. Add your secret in YAML format:
   ```yaml
   github:
     token: ghp_your_token_here

   api:
     key: your_api_key_here
   ```

3. Update `modules/darwin/sops.nix` to declare the new secret:
   ```nix
   sops.secrets."api/key" = {
     owner = "ldangelo";
     mode = "0400";
   };
   ```

4. Rebuild your system:
   ```bash
   darwin-rebuild switch --flake .
   ```

## Security Notes

- The encrypted `secrets.yaml` file is safe to commit to git
- Never commit unencrypted secrets
- Keep your age keys and SSH private keys secure
- The `.gitignore` is configured to only allow encrypted files
