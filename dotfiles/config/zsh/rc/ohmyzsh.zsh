#!/usr/bin/env zsh

VI_MODE_SET_CURSOR=true
plugins=(
    1password
    alias-finder
    dircycle
    direnv
    docker
    docker-compose
    dotnet
    emacs
    eza
    fzf
    git
    git-flow
    ssh
    starship
    sudo
    vi-mode
    z
    zsh-interactive-cd
    zsh-vi-mode
)


# ~/.zshrc

zstyle ':omz:plugins:alias-finder' autoload yes # disabled by default
zstyle ':omz:plugins:alias-finder' longer yes # disabled by default
zstyle ':omz:plugins:alias-finder' exact yes # disabled by default
zstyle ':omz:plugins:alias-finder' cheaper yes # disabled by default
