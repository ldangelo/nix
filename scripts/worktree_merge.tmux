#!/bin/bash
# tmux template for merging current worktree

if [ -z "$TMUX" ]; then
  echo "Not in tmux session"
  exit 1
fi

# Confirm merge
echo "Merging current branch into default branch"
echo "This will:"
echo "  1. Squash & rebase into default branch"
echo "  2. Delete the worktree"
echo "  3. Close this window"
echo
read -p "Continue? (y/N): " confirm

if [ "$confirm" = "y" ] || [ "$confirm" = "Y" ]; then
  wt merge
  if [ $? -eq 0 ]; then
    tmux display-message "✅ Merged and cleaned up"
    sleep 1
    tmux kill-window
  else
    tmux display-message "❌ Merge failed"
  fi
else
  tmux display-message "Cancelled merge"
fi
