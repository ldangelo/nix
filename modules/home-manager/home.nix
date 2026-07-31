{ config, pkgs, ... }:

{
  # direnv installed via Homebrew (nixpkgs direnv-2.37.1 fails: CGO not available in nix sandbox on macOS)

  # Create .envrc file that reads from sops secrets
  home.file.".envrc".text = ''
    secret() {
      local name="$1"
      if [ -r "$HOME/.config/sops-nix/secrets/$name" ]; then
        cat "$HOME/.config/sops-nix/secrets/$name"
      elif [ -r "/run/secrets/$name" ]; then
        cat "/run/secrets/$name"
      else
        printf ""
      fi
    }

#    export ANTHROPIC_API_KEY="$(secret anthropic_api_key)"
    export OPENROUTER_API_KEY="$(secret openrouter_api_key)"
    export SOPS_AGE_KEY_FILE="$HOME/.config/sops/age/keys.txt"
#    export OPENCLAW_GATEWAY_TOKEN="$(secret openclaw_gateway_token)"
#    export GITHUB_TOKEN="$(secret github/token)"
    export OPENAI_API_KEY="$(secret openai_api_key)"
    export CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS=1
    export MINIMAX_API_KEY="$(secret minimax_api_key)"
    export LITELLM_UI_PASSWORD="$(secret litellm_ui_password)"
    export LITELLM_MASTER_KEY="$(secret litellm_master_key)"
  '';
}

