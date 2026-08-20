#!/bin/bash
# tmux template for listing worktrees

if [ -z "$TMUX" ]; then
  echo "Not in tmux session"
  exit 1
fi

TMUX_SESSION_NAME=$(tmux display-message -p "#{session_name}")

# Show worktree list in a popup
tmux new-window -t $TMUX_SESSION_NAME -n "worktrees" \
  -c "$(pwd)" \
  "clear; echo '=== Worktrees ==='; echo; wt list; echo; read -p 'Press Enter to close...'"
