#!/usr/bin/env bash
# o-layout.sh
# Workspace O layout: Rize full-height center, others split evenly left/right
# Uses yabai --grid for precise float-based placement (no tree fighting).
#
# Grid reference: rows:cols:x:y:width:height (all 0-indexed positions/sizes)
# Trigger manually: shift+alt+r

SPACE_INDEX=$(yabai -m query --spaces | jq '.[] | select(.label == "O") | .index')
if [[ -z "$SPACE_INDEX" ]]; then
  osascript -e 'display notification "Space O not found" with title "yabai"'
  exit 1
fi

# Ensure float layout on space O
yabai -m config --space O layout float

# Query all windows on space O
WINDOWS=$(yabai -m query --windows --space "$SPACE_INDEX")

# Find Rize
RIZE=$(echo "$WINDOWS" | jq '.[] | select(.app == "Rize") | .id')
if [[ -z "$RIZE" ]]; then
  osascript -e 'display notification "Rize not found on space O" with title "yabai"'
  exit 1
fi

# All other windows
OTHER_IDS=($(echo "$WINDOWS" | jq -r '.[] | select(.app != "Rize") | .id'))
COUNT=${#OTHER_IDS[@]}

# Rize alone — center fullscreen
if [[ $COUNT -eq 0 ]]; then
  yabai -m window "$RIZE" --grid 1:1:0:0:1:1
  exit 0
fi

# Split others evenly left/right
LEFT_COUNT=$(( COUNT / 2 ))
RIGHT_COUNT=$(( COUNT - LEFT_COUNT ))
LEFT=("${OTHER_IDS[@]:0:$LEFT_COUNT}")
RIGHT=("${OTHER_IDS[@]:$LEFT_COUNT}")

# ── Rize: center column, full height ─────────────────────────────────────────
yabai -m window "$RIZE" --grid 1:3:1:0:1:1

# ── Left column ───────────────────────────────────────────────────────────────
for (( i=0; i<LEFT_COUNT; i++ )); do
  yabai -m window "${LEFT[$i]}" --grid "${LEFT_COUNT}:3:0:${i}:1:1"
done

# ── Right column ──────────────────────────────────────────────────────────────
for (( i=0; i<RIGHT_COUNT; i++ )); do
  yabai -m window "${RIGHT[$i]}" --grid "${RIGHT_COUNT}:3:2:${i}:1:1"
done

yabai -m window "$RIZE" --focus
