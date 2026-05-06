# Powerlevel10k configuration
# Based on catppuccin-mocha theme colors

# Use catppuccin mocha palette for powerlevel10k
typeset -g POWERLEVEL9K_MODE='powerline'
typeset -g POWERLEVEL9K_BACKGROUND='false'
typeset -g POWERLEVEL9K_TRANSIENT_PROMPT='always'
typeset -g POWERLEVEL9K_TIMESTAMP_FORMAT='%Y-%m-%d %H:%M:%S'

# Git status
typeset -g POWERLEVEL9K_VCS_BRANCH_ICON='branch'
typeset -g POWERLEVEL9K_VCS_UNTRACKED_ICON='?'
typeset -g POWERLEVEL9K_VCS_STAGED_ICON='+'
typeset -g POWERLEVEL9K_VCS_UNSTAGED_ICON='*'
typeset -g POWERLEVEL9K_VCS_COMMIT_ICON='git'
typeset -g POWERLEVEL9K_VCS_STASH_ICON='stash'
typeset -g POWERLEVEL9K_VCS_REMOTE_BRANCH_ICON='branch'

# Colors (Catppuccin Mocha)
typeset -g POWERLEVEL9K_BACKGROUND='false'
typeset -g POWERLEVEL9K_FOREGROUND='15'
typeset -g POWERLEVEL9K_ROOT_INDICATOR_BACKGROUND='0'
typeset -g POWERLEVEL9K_ROOT_INDICATOR_FOREGROUND='160'

# Left prompt segments
typeset -g POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(
    os_icon        # os icon
    vcs            # git status
    prompt_char    # prompt character
)

# Right prompt segments
typeset -g POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(
    status         # exit code
    command_execution_time  # command duration
    time           # current time
)

# Segment colors (Catppuccin Mocha)
typeset -g POWERLEVEL9K_OS_ICON_BACKGROUND='0'
typeset -g POWERLEVEL9K_OS_ICON_FOREGROUND='167'
typeset -g POWERLEVEL9K_VCS_BACKGROUND='30'
typeset -g POWERLEVEL9K_VCS_FOREGROUND='15'
typeset -g POWERLEVEL9K_PROMPT_CHAR_BACKGROUND='0'
typeset -g POWERLEVEL9K_PROMPT_CHAR_FOREGROUND='15'
typeset -g POWERLEVEL9K_STATUS_BACKGROUND='160'
typeset -g POWERLEVEL9K_STATUS_FOREGROUND='15'
typeset -g POWERLEVEL9K_COMMAND_EXECUTION_TIME_BACKGROUND='99'
typeset -g POWERLEVEL9K_COMMAND_EXECUTION_TIME_FOREGROUND='15'
typeset -g POWERLEVEL9K_TIME_BACKGROUND='30'
typeset -g POWERLEVEL9K_TIME_FOREGROUND='15'

# Catppuccin Mocha colors
# base:   #1e1e2e
# mantle: #181825
# crust:  #11111b
# text:   #cdd6f4
# subtext1: #bac2de
# subtext0: #a6adc8
# overlay2: #9399b2
# overlay1: #7f849c
# overlay0: #6c7086
# surface2: #585b70
# surface1: #45475a
# surface0: #313244
# base:   #1e1e2e
# rosewater: #f5e0dc
# flamingo: #f2cdcd
# pink: #f5c2e7
# mauve: #cba6f7
# red: #f38ba8
# maroon: #eba0ac
# peach: #fab387
# yellow: #f9e2af
# green: #a6e3a1
# teal: #94e2d5
# sky: #89dceb
# sapphire: #74c7ec
# blue: #89b4fa
# lavender: #b4befe
