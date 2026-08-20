{ config, pkgs, lib, isWorkstation ? true, ... }:

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
      "marcus/tap"
#      "d12frosted/emacs-plus"
      "felixkratz/formulae"
      "nikitabobko/tap"
      "dicklesworthstone/tap"
      "charmbracelet/tap"
      "bakks/bakks"
      "steipete/tap"
      "manaflow-ai/cmux"
      "acsandmann/tap"
      "stablyai/orca"
      "asheshgoplani/tap"  # agent-deck
    ];

    # Brew formulae not easily available in nixpkgs
    brews = [
      # Development Tools (not in nixpkgs)
      "aider"                          # AI pair programming
      "basedpyright"                   # Pyright fork with improvements
      "dicklesworthstone/tap/bv"        # Beads Viewer TUI (graph-aware issue triage)
      #"gastown"                        # Go-based agentic task runner (steveyegge)
      "evil-helix"                     # Helix editor soft fork
      "steipete/tap/imsg"              # iMessage/SMS CLI
      "steipete/tap/peekaboo"          # macOS UI automation CLI
      "ical-buddy"                     # Calendar CLI
      "igrep"                          # Interactive grep
      "jql"                            # JSON query language
      "lazyjj"                         # TUI for Jujutsu
      "nuget"                          # .NET package manager
      "opencode"                       # AI coding agent
      "marcus/tap/td"                  # Task/delegation CLI
      "pi-coding-agent"                # Pi AI coding agent
      "repomix"                        # Pack repo into AI-friendly file
      "swagger-codegen"                # OpenAPI code generator
      "worktrunk"                      # Git worktree manager (parallel agent workflows)
      "localstack"                     # Local AWS cloud emulator
      "asheshgoplani/tap/agent-deck"   # AI agent deck CLI

      # Shell & System Tools
      "bakks/bakks/butterfish"         # LLM command-line tool
      "oh-my-posh"                     # Prompt theme engine
      "charmbracelet/tap/crush"        # Terminal AI assistant
      "ttyd"                           # Share terminal over the web
      "vhs"                            # Record terminal sessions as GIF/video

      # golang
      "golang"

      "alot"                           # Notmuch mail client
      "notmuch"                        # Email indexing

      "direnv"                         # Shell env loader (nixpkgs build broken on macOS aarch64)
      "xpdf"                           # PDF viewer (insecure in nix)
      "cask"                           # Emacs dependency management
      "dockutil"                       # Dock management (Swift build broken in nixpkgs)
      "marksman"                       # Markdown language server (depends on .NET/Swift in nixpkgs)
      "dotnet@6"                       # .NET 6
      "flyctl"                         # Fly.io CLI
      "vi-sql"                  # Terminal UI for SQL databases with vim motions
    ] ++ lib.optionals isWorkstation [
      "fzf"
      "fd"
      "ripgrep"
      "bat"
      "delta"
      "tree"
      "the_silver_searcher"
      "z"
      "zoxide"
      "httpie"
      "yq"
      "jq"
      "kubectx"
      "skopeo"
      "gh"
      "helm"
      "kind"
      "minikube"
      "k9s"
      "kubectl"
    ];

    # macOS applications (casks) not in nixpkgs or better via homebrew
    casks = [
      "1password"
      "1password-cli"
      "alfred"
      "android-studio"
      "bartender"
      "brave-browser"
      "claude-code"
      "docker"
      "flux"
      "gemini"
      "ghostty"
      "iterm2"
      "karabiner-elements"
      "keyboardcleantool"
      "logi-options+"
      "microsoft-edge"
      "microsoft-teams"
      "notion"
      "obsidian"
      "openinterminal"
      "rectangle"
      "raycast"
      "safari-technology-preview"
#      "skype"  # DNS unreachable: download.skype.com
      "slack"
      "spotify"
      "tableplus"
      "telegram"
      "the-unarchiver"
      "visual-studio-code"
      "wireshark-app"
      "zoom"
    ] ++ lib.optionals isWorkstation [
      "fleet"
    ];

    # Mac App Store applications
    masApps = {
      "Fantastical" = 975937182;
      "OmniGraffle" = 361905126;
      "DEVONthink 3" = 906179395;
    };

    # Cleanup options
    onActivation = {
      autoUpdate = true;
      cleanup = "uninstall";
      upgrade = true;
    };
  };
}
