#!/usr/bin/env bash
SESSION="editor"
DIR="${1:-$(pwd)}"
tmux new-session -d -s "$SESSION" -c "$DIR"
tmux send-keys -t "$SESSION" "nvim ." Enter
tmux attach-session -t "$SESSION"
