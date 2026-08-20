#!/bin/bash
# tmux template for listing worktrees

if [ -z "$TMUX" ]; then
  echo "Not in tmux session"
  exit 1
fi

# Check if wt (worktrunk) is available
if ! command -v wt &> /dev/null; then
  tmux display-message "❌ worktrunk CLI (wt) not found in PATH"
  exit 1
fi

TMUX_SESSION_NAME=$(tmux display-message -p "#{session_name}")

# Show worktree list in a popup
# Quote $(pwd) to handle paths with spaces/special chars
tmux new-window -t "$TMUX_SESSION_NAME" -n "worktrees" \
  -c "$(pwd)" \
  "clear; echo '=== Worktrees ==='; echo; wt list; echo; read -p 'Press Enter to close...'"
