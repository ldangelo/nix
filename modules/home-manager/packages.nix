{ pkgs, lib, config, ... }:

let
  inherit (pkgs.stdenv) isDarwin isLinux;
in {
  # Explicitly opt out of managing ~/.npmrc; the installAgentMemory activation
  # script sets NPM_CONFIG_PREFIX explicitly and does not depend on this file.
  home.file.".npmrc".enable = false;

  home.activation.installAgentMemory = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    echo "Installing AgentMemory npm package..."
    export NPM_CONFIG_PREFIX="$HOME/.npm-global"
    mkdir -p "$NPM_CONFIG_PREFIX"
    PATH="${pkgs.nodejs}/bin:$PATH" ${pkgs.nodejs}/bin/npm install --global --prefix "$NPM_CONFIG_PREFIX" @agentmemory/agentmemory@0.9.24
  '';

  # Shared package manifest for the active home-manager profile.
  home.packages = with pkgs;
    [
      # Development Tools
      act
      ast-grep
      cargo-binstall
      cmake
      devbox
      glow
      helix
      jdt-language-server
      just
      maven
      nil
      openapi-generator-cli
      plantuml
      process-compose
      devenv
      trivy

      # Git Tools
      delta
      diffnav
      git-town
      glab
      jira-cli-go

      # Shell & Terminal Tools
      atuin
      bat
      oh-my-posh
      btop
      bun
      coreutils
      eza
      fasd
      fd
      sesh
      tmuxp
      mosh
      zoxide
      git-lfs
      htop
      jq
      lsd
      zsh-autosuggestions
      zsh-syntax-highlighting

      # Text Processing & Search
      markdown-toc
      markdownlint-cli
      markdownlint-cli2
      multimarkdown
      pandoc
      ripgrep
      platinum-searcher

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
      #      nodejs
      openjdk21
      postgresql
      python3
      ruby
      rustc

      # Language Servers (LSP)
      elixir-ls              # Elixir/Erlang Language Server
      omnisharp-roslyn       # C#/.NET Language Server
      # Language Version Managers
      nvm
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
      duf
      gnupg
      httpie
      hugo
      pipx
      scc
      uv
      virtualenv
      wget

      # Fonts
      source-code-pro
    ]
    ++ lib.optionals isLinux [
      direnv

      # GUI Applications
      rustdesk
      wakatime-cli
    ]
    ++ lib.optionals isDarwin [
      # Email & Communication
      afew
      cyrus_sasl
      cyrus-sasl-xoauth2
      himalaya
      mu
      neomutt
      pizauth

      # Development Tools
      antigravity
      ghostty-bin
      jankyborders

      # Shell & Terminal Tools
      terminal-notifier

      # Utilities
      choose-gui
      mas

      # GUI Applications
      aldente
      apparency
      discord
      duti
      mysides
      raycast
      shortcat
    ];

  systemd.user.services.rustdesk = lib.mkIf isLinux {
    Unit = {
      Description = "RustDesk remote desktop service";
      Documentation = [
        "https://rustdesk.com/docs/en/client/linux/"
        "https://rustdesk.com/docs/en/self-host/client-deployment/"
      ];
      After = [ "graphical-session.target" "network-online.target" ];
      Wants = [ "network-online.target" ];
      X-Restart-Triggers = [ pkgs.rustdesk ];
    };

    Service = {
      ExecStart = "${pkgs.rustdesk}/bin/rustdesk --service";
      Restart = "on-failure";
      RestartSec = 10;
    };

    Install.WantedBy = [ "default.target" ];
  };
}
