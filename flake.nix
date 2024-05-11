{
  description = "Leo's Nix Configuration";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    #    flake-utils.url = "github:numtide/flake-utils";

    # Home manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Darwin
    nix-darwin.url = "github:lnl7/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    # nixvim
#    nixvim.url = "github:nix-community/nixvim";
#    nixvim.inputs.nixpkgs.follows = "nixpkgs";

    # flake-parts
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";

    # nur: nix User Repository
    nur.url = "github:nix-community/NUR";
    nur.inputs.nixpkgs.follows = "nixpkgs";

    # Overlays

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
};

  outputs = inputs@{ self, nixpkgs, nur, home-manager, nix-darwin, ...}:
    let
      darwinConfiguration = { pkgs, ... }: {
        environment.systemPackages = [
          pkgs.vim
        ];

        services.nix-daemon.enable = true;

        nix.settings.experimental-features = "nix-command flakes";

        programs.zsh.enable = true;

        system.configurationRevision = self.rev or self.dirtyRev or null;

        system.stateVersion = 4;

        nixpkgs.hostPlatform = "x86_64-darwin";
        nixpkgs.config.allowUnfree = true;
      };

    in {
      darwinConfigurations."ldangelo" = nix-darwin.lib.darwinSystem {
        modules = [

          darwinConfiguration
          ./modules/darwin
          home-manager.darwinModules.home-manager {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              backupFileExtension = "backup";
              users.ldangelo.imports = [
#                nixvim.homeManagerModules.nixvim
                ./modules/home-manager
              ];
            };
          }
        ];
      };



      darwinPackages = self.darwinConfigurations."ldangelo".pkgs;
    };
}
