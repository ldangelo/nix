{ ... }: {
  users.users.ldangelo.home = "/Users/ldangelo";
  system.primaryUser = "ldangelo";

  nixpkgs.config.allowUnfree = true;

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  programs = {
    zsh.enable = true;
    gnupg.agent.enable = true;
  };

  imports = [
    ./system.nix
    ./homebrew.nix
    ./services.nix
    ./sops.nix
    ./pam.nix
  ];

  system.activationScripts.postActivation.text = ''
    if [ -f "/opt/homebrew/bin/brew" ]; then
      echo >&2 "Upgrading all Homebrew packages (including dependencies)..."
      PATH="/opt/homebrew/bin:$PATH" sudo --preserve-env=PATH --user=ldangelo --set-home env brew upgrade 2>&1 || true
    fi
  '';
}
