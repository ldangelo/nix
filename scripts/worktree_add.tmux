#!/bin/bash
# tmux template for adding new worktree

if [ -z "$TMUX" ]; then
  echo "Not in tmux session"
  exit 1
fi

# Check if wt (worktrunk) is available
if ! command -v wt &> /dev/null; then
  tmux display-message "❌ worktrunk CLI (wt) not found in PATH"
  exit 1
fi

# Prompt for branch name
read -p "Enter branch name (e.g., feature/user-auth): " branch_name

if [ -z "$branch_name" ]; then
  tmux display-message "❌ No branch name provided"
  exit 1
fi

# Create worktree using worktrunk
wt switch --create "$branch_name"

if [ $? -eq 0 ]; then
  tmux display-message "✅ Created worktree: $branch_name"
else
  tmux display-message "❌ Failed to create worktree: $branch_name"
fi
