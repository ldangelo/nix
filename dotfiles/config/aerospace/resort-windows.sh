#!/bin/bash
# AeroSpace window re-sort script
# Runs after startup to fix windows that opened before AeroSpace was ready
# Called from: after-startup-command with a sleep delay

A=/opt/homebrew/bin/aerospace

move() {
  $A list-windows --monitor all --app-bundle-id "$1" 2>/dev/null | cut -d"|" -f1 | tr -d " " | while read wid; do
    [ -n "$wid" ] && $A move-node-to-workspace "$2" --window-id "$wid" 2>/dev/null
  done
}

# Home (1)
move com.googlecode.iterm2 1

# Communication (C)
move com.tinyspeck.slackmacgap C
move us.zoom.xos C
move com.logmein.goto C
move com.apple.MobileSMS C
move com.hnc.Discord C
move ru.keepcoder.Telegram C
move com.readdle.SparkDesktop C

# Organization (O)
move com.flexibits.fantastical2.mac O
move md.obsidian O
move info.eurocomp.Timing-setapp O

# Entertainment (E)
move com.apple.Music E

# Trading (T)
move com.tradingview.tradingviewapp.desktop T

# Development (D)
move com.jetbrains.intellij D
