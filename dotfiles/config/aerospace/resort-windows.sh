#!/bin/bash
# AeroSpace window re-sort script
# Runs after startup to fix windows that opened before AeroSpace was ready
# Called from: after-startup-command with a sleep delay
#
# Five workspaces:
#   D Development | C Communication | N Notes | W Documents | P Personal

A=/opt/homebrew/bin/aerospace

move() {
  $A list-windows --monitor all --app-bundle-id "$1" 2>/dev/null | cut -d"|" -f1 | tr -d " " | while read wid; do
    [ -n "$wid" ] && $A move-node-to-workspace "$2" --window-id "$wid" 2>/dev/null
  done
}

# Development (D)
move com.github.wez.wezterm D
move com.electron.dockerdesktop D
move com.google.Chrome D

# Communication (C)
move com.microsoft.Outlook C
move com.apple.mail C
move com.tinyspeck.slackmacgap C
move com.microsoft.teams2 C
move com.apple.MobileSMS C

# Notes (N)
move md.obsidian N
move com.granola.app N
move com.anthropic.claudefordesktop N

# Documents (W)
move com.microsoft.Excel W
move com.apple.Preview W
move com.microsoft.Word W

# Personal (P)
move com.apple.Music P
move com.vivaldi.Vivaldi P
move com.tradingview.tradingviewapp.desktop P
