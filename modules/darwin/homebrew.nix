{ config, pkgs, ... }:

{
  # Homebrew configuration for packages not available in nixpkgs
  homebrew = {
    enable = true;

    # Taps that aren't in nixpkgs
    taps = [
#      "homebrew/cask"
#      "homebrew/core"
#      "homebrew/bundle"
#      "homebrew/services"
      "d12frosted/emacs-plus"
      "felixkratz/formulae"
      "nikitabobko/tap"
      "charmbracelet/tap"
      "bakks/bakks"
      "steipete/tap"
      "steveyegge/beads"
    ];

    # Brew formulae not easily available in nixpkgs
    brews = [
      # Development Tools (not in nixpkgs)
      "aider"                          # AI pair programming
      "basedpyright"                   # Pyright fork with improvements
      "bd"
#      "devbox"                         # Development environments
#      "devpod"                         # Development containers
      "evil-helix"                     # Helix editor soft fork
      "steipete/tap/imsg"              # iMessage/SMS CLI
      "steipete/tap/peekaboo"          # macOS UI automation CLI
      "ical-buddy"                     # Calendar CLI
      "igrep"                          # Interactive grep
      "jql"                            # JSON query language
      "lazyjj"                         # TUI for Jujutsu
      "nuget"                          # .NET package manager
      "opencode"                       # AI coding agent
      "repomix"                        # Pack repo into AI-friendly file
      "swagger-codegen"                # OpenAPI code generator

      # Shell & System Tools
      "bakks/bakks/butterfish"         # LLM command-line tool
      "charmbracelet/tap/crush"        # Terminal AI assistant
      "choose-gui"                     # Dotfile manager
      "evil-helix"                     # Helix soft fork
      "fileql"                         # SQL-like queries on files
      "ifstat"                         # Interface statistics
      "kanata"                         # Keyboard remapper
      "switchaudio-osx"                # Change audio source CLI
      "vfkit"                          # Virtualization framework CLI
      "openclaw-cli"
      "tailscale"
      # Email (not available on macOS via nix)
      "alot"                           # Notmuch mail client
      "notmuch"                        # Email indexing

      # Special packages with build requirements
      {
        name = "d12frosted/emacs-plus/emacs-plus@31";
        args = [
          "with-imagemagick"
          "with-mailutils"
          "with-no-frame-refocus"
        ];
      }
      # Window/UI management
      "felixkratz/formulae/borders"    # Window border system

      # Packages from original config
      "podman"                         # Container management
      "docker-compose"                 # Docker compose
      "chart-testing"                  # Helm chart testing
      "xpdf"                           # PDF viewer (insecure in nix)

      # Additional utilities
      "cask"                           # Emacs dependency management
      "dockutil"                       # Dock management (Swift build broken in nixpkgs)
      "marksman"                       # Markdown language server (depends on .NET/Swift in nixpkgs)
      "dotnet@6"                       # .NET 6
      "dotnet@8"                       # .NET 8 (Swift build broken in nixpkgs)
      "mpv"                            # Media player (Swift build broken in nixpkgs)
      "fisher"                         # Fish shell plugin manager
      "haskell-stack"                  # Haskell development
      "nvm"                            # Node version manager
      "watch"
      "yt-dlp"                         # YouTube downloader (curl-impersonate broken in nixpkgs on macOS 15)
    ];

    # macOS applications (casks) not in nixpkgs or better via homebrew
    casks = [
      "1password"
      "1password-cli"
      "aerospace"              # i3-like tiling window manager
      "aldente"                # Battery charge limiter
#      "alt-tab"                # Windows-like alt-tab
      "apparency"              # App inspector
      "kiro-cli"               # AI assistant
#      "ammonite"               # Tag visualizer
      "appcleaner"
      "arc"                    # Chromium browser
      "block-goose"            # AI agent
#      "felixkratz/formulae/borders"
      "claude-code"            # Terminal AI assistant
      "davmail-app"            # Exchange mail/calendar client
      "docker-desktop"
      "elgato-camera-hub"
      "elgato-control-center"
      "elgato-stream-deck"
      "elgato-wave-link"
      "fantastical"            # Calendar
      "font-cantarell"
      "font-fira-code"
      "font-fira-code-nerd-font"
      "font-source-code-pro"
      "git-credential-manager"
      "google-chrome"
      "gotomeeting"
      "grammarly-desktop"
      "granola"                # AI meeting notes
#      "hammerspoon"            # Desktop automation
#      "homerow"                # Keyboard shortcuts
#      "hookmark"               # Link and retrieve info
      "iterm2"
      "itermai"                # AI for iTerm2
      "itermbrowserplugin"     # Browser in iTerm2
      "jetbrains-toolbox"
      "karabiner-elements"     # Keyboard customizer
      "launchcontrol"          # Service manager
      "lens"                   # Kubernetes IDE
#      "limitless"              # AI transcription
      "linear-linear"          # Project management
      "logseq"                 # Knowledge management
      "mactex-no-gui"          # TeX distribution
      "meld"                   # Visual diff
      "microsoft-auto-update"
      "microsoft-teams"
      "mouseless@preview"      # Mouse control via keyboard
      "obsidian"
      "ollama-app"             # Local LLMs
      "openclaw"
      "postgres-unofficial"    # Postgres.app
      "postman"
      "postman-cli"
      "proxyman"               # HTTP debugging proxy
      "raycast"                # Launcher and productivity
      "readdle-spark"          # Email client
      "repo-prompt"            # Prompt generation
      "rider"
      "rize"
      "setapp"
      "sf-symbols"
#      "shortcat"               # Keyboard navigation
      "slack"
      "sourcetree"             # Git GUI
#      "stats"                  # System monitor
      "superhuman"             # Email client
#      "tabtab"                 # Window/tab manager
      "tailscale-app"
      "todoist-app"
      "tradingview"
#      "vscodium"               # VS Code without telemetry
      "wakatime"
#      "warp-terminal"          # Terminal with AI
      "wezterm"
      "witsy"                  # BYOK AI assistant
      "zed"                    # Code editor
      "zoom"
      #
      # yubi key
      "yubico-authenticator"
      #      "yubico-yubikey-manager"
    ];

    # Mac App Store applications
    masApps = {
#      "1Password for Safari" = ;
#      "AdGuard for Safari" = 1440147259;
#      "Desktop App for Jira" = 6572290663;
#      "Everhour" = 1539652800;
#      "GarageBand" = 682658836;
#      "Grammarly for Safari" = 1462114288;
#      "iMovie" = 408981434;
#      "Keynote" = 409183694;
#      "MarkChart" = 6475648822;
#      "Notion Web Clipper" = 1559269364;
#      "Numbers" = 409203825;
#      "Obsidian Web Clipper" = 6720708363;
#      "Omi" = 6502156163;
#      "Pages" = 409201541;
#      "Raycast Companion" = 6738274497;
#      "TestFlight" = 899247664;
#      "Toggl Track" = 1291898086;
#      "Tracking Time | Button" = 1587766224;
#      "Userscripts" = 1463298887;
#      "Vimkey" = 1585682577;
#      "Windows App" = 1295203466;
    };

    # Cleanup options
    onActivation = {
      cleanup = "zap";        # Uninstall packages not listed
      autoUpdate = true;      # Auto-update Homebrew
      upgrade = true;         # Auto-upgrade packages
    };
  };
}
