{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    # Development Tools
    atuin
    act                    # Run GitHub Actions locally
    # aider - not in nixpkgs, available via brew
    ast-grep               # Code searching, linting, rewriting
    # basedpyright - not in nixpkgs
    cargo-binstall         # Binary installation for rust projects
    cmake                  # Cross-platform make
    claude-code
    claude-monitor
    # devbox - not in nixpkgs
    # devpod - not in nixpkgs
    devenv
    gradle                 # Build automation tool
    google-chrome
    helix
    # jdtls - use jdt-language-server
    jankyborders
    jdt-language-server    # Java language server
    just                   # Command runner
    lazygit                # Simple terminal UI for git
    # lazyjj - not in nixpkgs yet
    maven                  # Java project management
    neovim
    # nuget - not in standard nixpkgs
    # openapi-generator - not directly available, use openapi-generator-cli
    openapi-generator-cli  # Generate clients/servers from OpenAPI
    plantuml               # Draw UML diagrams
    # repomix - not in nixpkgs
    # swagger-codegen - not in nixpkgs
    trivy                  # Vulnerability scanner

    # Git Tools
    gh                     # GitHub CLI
    git
    git-town               # High-level Git interface
    glab                   # GitLab CLI
    jujutsu                # Jujutsu version control (package name is jujutsu not jj)

    # Shell & Terminal Tools
    atuin                  # Improved shell history
    autojump               # Jump to frequently used directories
    btop                   # Resource monitor
    coreutils              # GNU utilities
    direnv                 # Load/unload env vars based on pwd
    eza                    # Modern ls replacement
    fasd                   # Quick access to files/directories
    fd                     # Simple alternative to find
    fzf                    # Fuzzy finder
    htop                   # Process viewer
    # ifstat - not in nixpkgs
    starship               # Cross-shell prompt
    terminal-notifier      # macOS notifications from CLI
    zoxide                 # Smarter cd command
    zsh-autosuggestions
    zsh-syntax-highlighting

    # Text Processing & Search
    # igrep - not in nixpkgs
    jq                     # JSON processor
    # jql - not in nixpkgs
    marksman               # Markdown language server
    multimarkdown          # Markdown converter
    pandoc                 # Document converter
    ripgrep                # Fast search
    silver-searcher        # Code search (ag)

    # Email & Communication
    # alot - Linux only, not available on macOS
    mu                     # Email search tool
    msmtp                  # SMTP client
    neomutt                # Email client
    notmuch                # Email indexing
    # notmuch-mutt - included with notmuch

    # Cloud & Infrastructure
    awscli2                # AWS CLI (v2)
    docker-credential-helpers
#    flyctl                 # Fly.io CLI
    gitlab-runner          # GitLab CI runner
    kubernetes-helm        # Kubernetes package manager
    kubectl                # Kubernetes CLI
    kubeconform            # Kubernetes manifest validator
    temporal               # Temporal workflow CLI
    terraform              # Infrastructure as code

    # Programming Languages & Runtimes
    dotnet-sdk_8           # .NET Core 8
    elixir                 # Elixir language
    openjdk                # Java
    openjdk21              # Java 21
    nodejs                 # Node.js
    postgresql             # PostgreSQL (use postgresql not postgresql_17)
    python3                # Python
    ruby                   # Ruby
    rustc                  # Rust compiler
    cargo                  # Rust package manager

    # Language Version Managers
    rbenv                  # Ruby version manager
    # Note: nvm is shell-based, configure in shell.nix

    # Build Tools & Libraries
    autoconf
    automake
    binutils
    clang-tools            # For clang-format
    cyrus_sasl             # SASL library
    cyrus-sasl-xoauth2     # OAuth2 for SASL (from overlay)
    graphviz               # Graph visualization
    harfbuzz               # Text shaping
    ispell                 # Spell checker
    libjpeg                # JPEG library
    # libvterm - not available on macOS
    libtool
    luarocks               # Lua package manager
    pkg-config             # Package config (not pkgconf)
    zlib                   # Compression library

    # Media & Graphics
    chafa                  # Graphics renderer
    mpv                    # Media player
    viu                    # Terminal image viewer
    w3m                    # Text browser
    # xpdf - marked as insecure, use brew or alternative

    # Utilities
    duf                    # Disk usage utility
    # fileql - not in nixpkgs
    gnupg                  # GPG encryption
    httpie                 # HTTP client
    hugo                   # Static site generator
    # kanata - not in standard nixpkgs
    mas                    # Mac App Store CLI
    pipx                   # Execute Python packages
    scc                    # Code counter
    # switchaudio-osx - not in nixpkgs, use brew
    uv                     # Fast Python package installer
    # vfkit - not in nixpkgs
    virtualenv             # Python virtual environments
    wget                   # File retriever

    # Fonts
    #nerdfonts
    source-code-pro

    # GUI Applications (available in nixpkgs)
    # aldente - not in nixpkgs, use cask
    # apparency - not in nixpkgs
    discord
    dockutil              # Dock management
    duti                  # Default apps manager
    # mysides - not in nixpkgs
    # raycast - use cask
    # shortcat - not in nixpkgs
    # swiftdefaultapps - not in nixpkgs
    vscode
    # warp-terminal - use cask

    # Already in default.nix, kept for reference
    # cyrus-sasl-xoauth2 (from overlay)
  ];
}
