# tmuxai configuration.
#
# tmuxai (Homebrew formula, see modules/darwin/homebrew.nix) reads
# ~/.config/tmuxai/config.yaml. We point it at the LiteLLM proxy (the same
# "llmproxy in docker" that pi/omp use — see pi-models.json) at
# http://gx10-1:14000/v1, authenticating with the litellm_master_key sops
# secret.
#
# The config is rendered by sops-nix (NOT home-manager's xdg.configFile) so
# the master key is injected at activation into a 0600 regular file, never
# landing in the world-readable /nix/store or in git. This mirrors the
# existing wakatime.cfg template in ./sops.nix. A regular file (not a
# /nix/store symlink) also lets tmuxai write its debug/ dir and honor runtime
# `/config set` without EACCES.
#
# tmuxai has no command-substitution key syntax (no `!cmd`), so the file-based
# secret injection is the only clean way to keep the key out of the store.

{ config, ... }:

let
  homeDir = config.home.homeDirectory;
  baseUrl = "http://localhost:4000/v1";
  masterKey = config.sops.placeholder.litellm_master_key;

  # tmuxai treats provider "openrouter" as a generic OpenAI-compatible
  # chat-completions client when base_url is set (see its config.example.yaml
  # local-llama entry), which is exactly what LiteLLM exposes.
  # Explicit two/four-space indentation baked in via a plain (non-indented)
  # string: this value is interpolated verbatim under `models:`, and Nix does
  # not re-indent multi-line interpolations. A `''` block here would strip its
  # own leading indent and break the nesting. Two spaces for the model name,
  # four for its fields.
  model = name:
    "  ${name}:\n"
    + "    provider: \"openrouter\"\n"
    + "    model: \"${name}\"\n"
    + "    api_key: \"${masterKey}\"\n"
    + "    base_url: \"${baseUrl}\"";
in
{
  sops.templates."tmuxai-config.yaml" = {
    path = "${homeDir}/.config/tmuxai/config.yaml";
    mode = "0600";
    content = ''
      # Managed by nix (modules/home-manager/tmuxai.nix). Edits are overwritten
      # on the next home-manager activation.

      # If empty uses the first model alphabetically.
      default_model: "qwen"

      # Models served by the LiteLLM proxy (see pi-models.json).
      models:
      ${model "qwen"}
      ${model "qwen38"}
      ${model "ornith"}
    '';
  };
}
