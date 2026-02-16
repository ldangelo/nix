#!/usr/bin/env bash
SESSION="simple"
tmux new-session -d -s "$SESSION" -c "${1:-$(pwd)}"
tmux attach-session -t "$SESSION"
