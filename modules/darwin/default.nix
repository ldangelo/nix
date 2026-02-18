{ pkgs, ... }: {
  # here go the darwin preferences and config items
  users.users.ldangelo.home = "/Users/ldangelo";
  system.primaryUser = "ldangelo";
#  system.defaults.dock.mru_space = false;  # do not rearrange spaces

  # Allow unfree packages (claude-code, google-chrome, vscode, etc.)
  nixpkgs.config.allowUnfree = true;

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  programs = {
    zsh.enable = true;
    gnupg.agent.enable = true;
  };

#   fonts.fonts = [ nixpkgs.source-code-pro ];
#  services.sketchybar.enable = true;
#  services.sketchybar.package = pkgs.sketchybar;

  imports = [
    ./system.nix
    #./yabai/default.nix
    #./skhd/default.nix
    ./homebrew.nix      # Migrated from Brewfile
    ./services.nix      # Launchd services (kanata, karabiner, etc.)
    ./sops.nix          # sops-nix secrets management
    ./pam.nix           # Touch ID and Apple Watch for sudo
  ];


  # Upgrade all Homebrew packages (including dependencies) during deploy
  system.activationScripts.postActivation.text = ''
    if [ -f "/opt/homebrew/bin/brew" ]; then
      echo >&2 "Upgrading all Homebrew packages (including dependencies)..."
      PATH="/opt/homebrew/bin:$PATH" sudo --preserve-env=PATH --user=ldangelo --set-home env brew upgrade 2>&1 || true
    fi
  '';
}
