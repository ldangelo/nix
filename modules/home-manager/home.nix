{ config, pkgs, ... }:

{
  home.packages = [ pkgs.direnv ];

  # Create .envrc file that reads from sops secrets
  home.file.".envrc".text = ''
#    export ANTHROPIC_API_KEY="$(cat /run/secrets/anthropic_api_key 2>/dev/null || echo "")"
    export OPENROUTER_API_KEY="$(cat /run/secrets/openrouter_api_key 2>/dev/null || echo "")"
    export SOPS_AGE_KEY_FIlE=~/.config/sops/age/keys.txt
    export OPENCLAW_GATEWAY_TOKEN="$(cat /run/secrets/openclaw_gateway_token 2>/dev/null || echo "")"
    export OPENAI_API_KEY="$(cat /run/secrets/openai_api_key 2>/dev/null || echo "")"
  '';
}

