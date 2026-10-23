#!/usr/bin/env bash
# One-time: copy secrets from the sops file into 1Password items, so chezmoi can
# render them (see home/dot_config/private_secrets/). Run it on a machine where
# sops + the age key still work (i.e. BEFORE removing Nix / sops).
#
#   VAULT=Private ./scripts/migrate-secrets-to-1password.sh [--dry-run]
#
# Each sops key becomes an item with that name and a `credential` field. Values
# are piped sops -> op and never printed. Existing items are skipped.
set -euo pipefail
VAULT="${VAULT:-Private}"
DRY=0; [[ "${1:-}" == "--dry-run" ]] && DRY=1
SECRETS_FILE="$(cd "$(dirname "$0")/.." && pwd)/secrets/secrets.yaml"
export SOPS_AGE_KEY_FILE="${SOPS_AGE_KEY_FILE:-$HOME/.config/sops/age/keys.txt}"
command -v sops >/dev/null || { echo "sops not found (install: brew install sops)" >&2; exit 1; }
command -v op   >/dev/null || { echo "op not found" >&2; exit 1; }
command -v jq   >/dev/null || { echo "jq not found" >&2; exit 1; }
op vault list >/dev/null 2>&1 || { echo "op is not signed in / app integration off" >&2; exit 1; }

# create <title> <value>: the value goes to op on stdin as a JSON template, not argv
create() {
  jq -n --arg t "$1" --arg v "$2" \
    '{title:$t, category:"API_CREDENTIAL", fields:[{id:"credential", type:"CONCEALED", label:"credential", value:$v}]}' \
    | op item create --vault "$VAULT" - >/dev/null
}

# sops key -> item name (identical, except the nested github keys)
keys=(
  "anthropic_api_key" "openrouter_api_key" "openai_api_key" "minimax_api_key"
  "gemini_api_key" "litellm_ui_password" "litellm_master_key" "litellm_proxy_master_key"
  "mac_mail_key" "fortium_mail_key" "wakatime_api_key" "todoist_api_key"
  "openclaw_gateway_token" "openclaw_telegram_bot_token"
)
for k in "${keys[@]}"; do
  if op item get "$k" --vault "$VAULT" >/dev/null 2>&1; then echo "skip   $k (exists)"; continue; fi
  if (( DRY )); then echo "would create $k in $VAULT"; continue; fi
  val="$(sops -d --extract "[\"$k\"]" "$SECRETS_FILE")" || { echo "FAILED to read $k" >&2; continue; }
  create "$k" "$val"
  unset val
  echo "created $k"
done
# nested keys: github/token, github/fortium
for pair in "github/token:github_token" "github/fortium:github_fortium_token"; do
  k="${pair%%:*}"; title="${pair##*:}"
  if op item get "$title" --vault "$VAULT" >/dev/null 2>&1; then echo "skip   $title (exists)"; continue; fi
  if (( DRY )); then echo "would create $title in $VAULT"; continue; fi
  val="$(sops -d --extract "[\"${k%%/*}\"][\"${k##*/}\"]" "$SECRETS_FILE")" || { echo "FAILED to read $k" >&2; continue; }
  create "$title" "$val"
  unset val
  echo "created $title"
done
