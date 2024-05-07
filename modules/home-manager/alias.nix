{ pkgs, misc, ... }: {
	home.shellAliases = {
	  "darwin-build" = "pushd ~/.config/nix-darwin; darwin-rebuild build --flake .#ldangelo; popd";
	  "darwin-switch" = "pushd ~/.config/nix-darwin; darwin-rebuild switch --flake .#ldangelo; popd";
    "nix-upgrade" = "sudo nix-env --install --file '<nixpkgs>' --attrs nix -I nixpkgs=channel:nixpkgs-unstable;sudo launchctl remove org.nixos.nix-daemon; sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist";
	};
}
