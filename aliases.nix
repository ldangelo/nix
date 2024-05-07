	{ pkgs, misc, ... }: {
	   home.shellAliases = {
	    "darwin-build" = "pushd ~/.config/nix-darwin; darwin-rebuild --flake .#ldangelo build; popd";
	    "hm-switch" = "pushd ~/.config/nix-darwin; darwin-rebuild --flake .#ldangelo switch; popd";
	    };
}
