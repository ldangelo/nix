{ config, pkgs, inputs,  ... }: {
  imports = [
    # Program configurations with native home-manager support
    ./git.nix           # Git configuration
    ./jujutsu.nix       # Jujutsu (jj) configuration
    ./nvim              # Neovim configuration
    ./emacs
    ./wezterm.nix       # WezTerm configuration
    ./qutebrowser       # Qutebrowser configuration
    ./programs.nix      # Other program configs (fzf, direnv, starship)
    ./packages.nix      # Migrated from Brewfile

    # Dotfiles from chezmoi
    ./dotfiles-chezmoi.nix  # All chezmoi-managed dotfiles

    # Legacy modules (keeping for reference)
    ./dotfiles          # Old dotfiles module
    ./path.nix
    ./shell.nix
    ./user.nix
    ./alias.nix
    ./nix-search-tv.nix
    ./home.nix
    #     ./trash
    ./borders
    ./zoxide.nix
    ./zellij
  ];


  # packages are just installed (no configuration applied)
  # programs are installed and configuration applied to dotfiles
  home = {
    username = "ldangelo";
    homeDirectory = "/Users/ldangelo";
    packages = with pkgs; [
      #       nixVersions.latest

      #       ./spacevim.nix
      # user selected packages
      pkgs.neovim
      pkgs.cyrus_sasl
      pkgs.cyrus-sasl-xoauth2
      #       pkgs.isync this package does not support oauth
      pkgs.devbox
      pkgs.bat
#      pkgs.devpod
      pkgs.pizauth
      pkgs.helix
      pkgs.raycast
#      pkgs.warp-terminal
      pkgs.lazygit
#      pkgs.nerd-fonts
      pkgs.source-code-pro
      pkgs.aldente
      pkgs.apparency
      #      pkgs.apple-sdk
      pkgs.binutils
      pkgs.dockutil
      pkgs.duti
      #      pkgs.macfuse
      #pkgs.mas
      pkgs.mysides
      #      pkgs.openwith
      pkgs.shortcat
      pkgs.swiftdefaultapps
      #      pkgs.trash
      #      pkgs.utm
      #      pkgs.xcode

      #      pkgs.emacs-macport
      #    pkgs.yabai
      pkgs.discord
      # Fleek Bling
      pkgs.git
      pkgs.htop
      pkgs.github-cli
      pkgs.glab
      pkgs.gnupg
      pkgs.fzf
      pkgs.ripgrep
      pkgs.vscode
      pkgs.just
      pkgs.silver-searcher
      pkgs.jq
      pkgs.fasd
      pkgs.zoxide
      pkgs.eza
      pkgs.scc
      pkgs.duf
      pkgs.just
    ];
  };

  catppuccin = {
    enable = true;
    atuin.enable = true;
    bat.enable = true;
    btop.enable = true;
    chromium.enable = true;
    fzf.enable = true;
    lazygit.enable = true;
    fish.enable = true;
    nushell.enable = true;
    starship.enable = true;
    wezterm.enable = true;
    nvim.enable = true;
    vscode.enable = true;
    zsh-syntax-highlighting.enable = true;
    qutebrowser.enable = true;
  };
  fonts.fontconfig.enable = true;

  # Create .envrc file with API keys from sops secrets
#   home.file.".envrc".text = ''
#     # API keys loaded from sops secrets
#     export ANTHROPIC_API_KEY="$(cat /run/secrets/anthropic_api_key 2>/dev/null || echo "")"
#     export OPENROUTER_API_KEY="$(cat /run/secrets/openrouter_api_key 2>/dev/null || echo "")"
#     export SOPS_AGE_KEY_FIlE=~/.config/sops/age/keys.txt
# #
#     # Commenting these out to use the gh auth workflow instead.
#     #
#     #
#     #export GITHUB_TOKEN="$(cat /run/secrets/github/token 2>/dev/null || echo "")"
#     #export GITHUB_FORTIUM_TOKEN="$(cat /run/secrets/github/fortium 2>/dev/null || echo "")"
#   '';
#
  # To figure this out (in-case it changes) you can comment out the line and see what version it expected.
  home.stateVersion = "22.11";

  # Let Home Manager install and manage itself
  programs.home-manager.enable = true;
}
