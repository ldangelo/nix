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

  # Homebrew is invoked as the user (not root) because cask/font/app installs
  # target $HOME and launchctl services live in the user's gui domain.
  # Inner sudo needs -A so brew's own nested sudo (e.g. `sudo launchctl`
  # during service removal) reuses the osascript askpass dialog instead of
  # blocking on a terminal `Password:` prompt.
  # CI=1 suppresses Homebrew's interactive `==> Proceed? [y/n]` prompt and
  # auto-confirms tap-trust. HOMEBREW_NO_UPGRADE_AUTO_UPDATES_CASKS=1 skips
  # auto-upgrade of casks flagged `auto_updates true` (run `brew upgrade
  # --greedy` manually to pick those up).
  system.activationScripts.postActivation.text = ''
    if [ -f "/opt/homebrew/bin/brew" ]; then
      echo >&2 "Upgrading all Homebrew packages (including dependencies)..."
      SUDO_ASKPASS=/Users/ldangelo/.local/bin/sudo-askpass \
        sudo -A --preserve-env=PATH,CI,HOMEBREW_NO_ENV_HINTS,HOMEBREW_NO_UPGRADE_AUTO_UPDATES_CASKS \
          --user=ldangelo --set-home \
        env CI=1 HOMEBREW_NO_ENV_HINTS=1 HOMEBREW_NO_UPGRADE_AUTO_UPDATES_CASKS=1 \
          brew upgrade 2>&1 || true
    fi
  '';
}
