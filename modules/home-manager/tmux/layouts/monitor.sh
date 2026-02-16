#!/usr/bin/env bash
SESSION="monitor"
tmux new-session -d -s "$SESSION"
tmux send-keys "htop" Enter
tmux split-window -h
tmux send-keys "watch -n 2 df -h" Enter
tmux select-pane -t 0
tmux split-window -v
tmux send-keys "watch -n 2 netstat -an" Enter
tmux select-pane -t 2
tmux split-window -v
# Leave bottom-right pane as shell for logs
tmux select-pane -t 0
tmux attach-session -t "$SESSION"
