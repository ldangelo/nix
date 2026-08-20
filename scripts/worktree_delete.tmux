#!/bin/bash
# tmux template for deleting current worktree

if [ -z "$TMUX" ]; then
  echo "Not in tmux session"
  exit 1
fi

# Check if wt (worktrunk) is available
if ! command -v wt &> /dev/null; then
  tmux display-message "❌ worktrunk CLI (wt) not found in PATH"
  exit 1
fi

# Get current branch name for confirmation
current_branch=$(wt branch 2>/dev/null || git branch --show-current)

# Confirm delete with branch name
echo "Delete current worktree: ${current_branch:-unknown}"
echo "(branch will be deleted if merged)"
echo
read -p "Continue? (y/N): " confirm

if [ "$confirm" = "y" ] || [ "$confirm" = "Y" ]; then
  wt remove
  if [ $? -eq 0 ]; then
    tmux display-message "✅ Deleted worktree: ${current_branch}"
  else
    tmux display-message "❌ Delete failed"
  fi
else
  tmux display-message "Cancelled delete"
fi
