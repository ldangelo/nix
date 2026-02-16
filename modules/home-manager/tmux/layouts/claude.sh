#!/usr/bin/env bash
SESSION="claude"
DIR="${1:-$(pwd)}"
tmux new-session -d -s "$SESSION" -c "$DIR"
tmux send-keys "nvim ." Enter
tmux split-window -h -p 50 -c "$DIR"
tmux send-keys "claude --continue" Enter
tmux select-pane -t 0
tmux attach-session -t "$SESSION"
