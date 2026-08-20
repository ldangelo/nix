#!/bin/bash
# tmux template for deleting current worktree

if [ -z "$TMUX" ]; then
  echo "Not in tmux session"
  exit 1
fi

# Confirm delete
echo "Delete current worktree (branch will be deleted if merged)"
echo
read -p "Continue? (y/N): " confirm

if [ "$confirm" = "y" ] || [ "$confirm" = "Y" ]; then
  wt remove
  if [ $? -eq 0 ]; then
    tmux display-message "✅ Deleted worktree"
  else
    tmux display-message "❌ Delete failed"
  fi
else
  tmux display-message "Cancelled delete"
fi
