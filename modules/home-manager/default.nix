{ ... }: {
  imports = [
    ./git.nix
    ./jujutsu.nix
    ./nvim
    ./emacs
    ./wezterm.nix
    ./qutebrowser
    ./lazygit.nix
    ./programs.nix
    ./packages.nix
    ./dotfiles-chezmoi.nix
    ./dotfiles
    ./path.nix
    ./shell.nix
    ./ssh.nix
    ./user.nix
    ./alias.nix
    ./nix-search-tv.nix
    ./home.nix
    ./sops.nix
    ./borders
    ./zoxide.nix
    ./tmux
    ./ghostty.nix
    ./yazi.nix
    ./mutable-config.nix
    ./ai-worktrees.nix
    ./tmuxai.nix
    ./rift-plugins.nix
  ];

  home = {
    username = "ldangelo";
    homeDirectory = "/Users/ldangelo";
  };

  catppuccin = {
    enable = true;
    autoEnable = true;
    atuin.enable = true;
    bat.enable = true;
    btop.enable = true;
    chromium.enable = true;
    fzf.enable = true;
    lazygit.enable = true;
    fish.enable = true;
    nushell.enable = true;
    wezterm.enable = true;
    nvim.enable = true;
    # vscode.enable = true;
    zsh-syntax-highlighting.enable = true;
    qutebrowser.enable = true;
    tmux.enable = true;
    yazi.enable = true;
  };
  # Programs that rewrite their own config (or lock it in place) — see mutable-config.nix.
  mutableConfig.xdgFiles = [
    "worktrunk/config.toml"
    "gh/config.yml"
    "gh-dash/config.yml"
    "yazi/yazi.toml"
    "yazi/keymap.toml"
    "yazi/theme.toml"
    "yazi/init.lua"
  ];
  fonts.fontconfig.enable = true;
  home.stateVersion = "22.11";

  programs.home-manager.enable = true;
}
