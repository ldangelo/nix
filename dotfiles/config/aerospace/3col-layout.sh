#!/usr/bin/env bash
# 3col-layout.sh
# Arranges the focused workspace into:
#   [left col: tiles vertical] | [center: focused window] | [right col: tiles vertical]
#
# The currently focused window is placed in the center.
# Other windows are split evenly left/right.
#
# Requires at least 3 windows.

notify() {
  osascript -e "display notification \"$1\" with title \"AeroSpace\""
}

WORKSPACE=$(aerospace list-workspaces --focused)
FOCUSED=$(aerospace list-windows --focused --format '%{window-id}' 2>/dev/null | head -1)

# Get all OTHER windows in workspace (not focused)
OTHERS=()
while IFS= read -r line; do
  [[ -n "$line" && "$line" != "$FOCUSED" ]] && OTHERS+=("$line")
done < <(aerospace list-windows --workspace "$WORKSPACE" --format '%{window-id}' 2>/dev/null)

TOTAL=$(( ${#OTHERS[@]} + 1 ))

if [[ $TOTAL -lt 3 ]]; then
  notify "Need at least 3 windows for 3-column layout (found $TOTAL)"
  exit 1
fi

# Split others evenly: lower half → left, upper half → right
LEFT_COUNT=$(( ${#OTHERS[@]} / 2 ))
LEFT=("${OTHERS[@]:0:$LEFT_COUNT}")
RIGHT=("${OTHERS[@]:$LEFT_COUNT}")

# Flatten to a clean horizontal row
aerospace flatten-workspace-tree

# Move left windows to far left
for id in "${LEFT[@]}"; do
  aerospace focus --window-id "$id"
  for (( i=0; i<TOTAL; i++ )); do
    aerospace move left 2>/dev/null || true
  done
done

# Move right windows to far right
for id in "${RIGHT[@]}"; do
  aerospace focus --window-id "$id"
  for (( i=0; i<TOTAL; i++ )); do
    aerospace move right 2>/dev/null || true
  done
done

# Stack left column vertically (if 2+ windows)
if [[ ${#LEFT[@]} -ge 2 ]]; then
  aerospace focus --window-id "${LEFT[0]}"
  aerospace join-with left
  aerospace layout tiles vertical
fi

# Stack right column vertically (if 2+ windows)
if [[ ${#RIGHT[@]} -ge 2 ]]; then
  aerospace focus --window-id "${RIGHT[0]}"
  aerospace join-with right
  aerospace layout tiles vertical
fi

# Balance column widths
aerospace balance-sizes

# Return focus to the center window
aerospace focus --window-id "$FOCUSED"
