{ pkgs, misc, ... }: {
	home.shellAliases = {
	  "darwin-switch" = "pushd ~/.config/nix; just deploy; popd";
	  "darwin-debug" = "pushd ~/.config/nix; just debug; popd";
	  "darwin-upgrade" = "pushd ~/.config/nix; just up; popd";
	};
}
