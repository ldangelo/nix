#!/usr/bin/env bash
SESSION="dev"
DIR="${1:-$(pwd)}"
tmux new-session -d -s "$SESSION" -c "$DIR"
tmux send-keys -t "$SESSION" "nvim ." Enter
tmux split-window -h -p 30 -c "$DIR"
tmux split-window -v -c "$DIR"
tmux send-keys "lazygit" Enter
tmux select-pane -t 0
tmux attach-session -t "$SESSION"
