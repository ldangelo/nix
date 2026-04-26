{ pkgs, ... }:

{
  # Shared package manifest for the active home-manager profile.
  home.packages = with pkgs; [
    # Development Tools
    act
    # antigravity  # removed: preFixup references glibc-nolibgcc, broken on aarch64-darwin
    ast-grep
    cargo-binstall
    claude-monitor
    cmake
    devbox
    glow
    ghostty-bin
    helix
    jankyborders
    jdt-language-server
    just
    maven
    nil
    # openapi-generator-cli  # nixpkgs hash mismatch on 7.21.0 patch; available via brew
    plantuml
    process-compose
    telegram-desktop
    devenv
    trivy

    # Git Tools
    delta
    gh
    git-town
    glab
    jira-cli-go

    # Shell & Terminal Tools
    atuin
    tmuxai
    bat
    btop
    bun
    coreutils
    eza
    fasd
    fd
    git-lfs
    htop
    jq
    lsd
    terminal-notifier
    zsh-autosuggestions
    zsh-syntax-highlighting

    # Text Processing & Search
    markdown-toc
    markdownlint-cli
    markdownlint-cli2
    marp-cli
    multimarkdown
    pandoc
    ripgrep
    silver-searcher

    # Email & Communication
    afew
    cyrus_sasl
    cyrus-sasl-xoauth2
    himalaya
    mu
    neomutt
    pizauth

    # Cloud & Infrastructure
    awscli2
    cloudflared
    docker-credential-helpers
    gitlab-runner
    kubeconform
    kubectl
    kubernetes-helm
    temporal
    terraform

    # Programming Languages & Runtimes
    cargo
    elixir
    nodejs
    openjdk21
    postgresql
    python3
    ruby
    rustc

    # Language Version Managers
    rbenv

    # Build Tools & Libraries
    autoconf
    automake
    binutils
    clang-tools
    graphviz
    harfbuzz
    ispell
    libjpeg
    libtool
    luarocks
    pkg-config
    zlib

    # Media & Graphics
    chafa
    viu
    w3m

    # Utilities
    choose-gui
    duf
    gnupg
    httpie
    hugo
    mas
    pipx
    scc
    uv
    virtualenv
    wget

    # GUI Applications
    aldente
    apparency
    discord
    duti
    mysides
    raycast
    shortcat
  ];
}
