{
  description = "Leo's Nix Configuration";

  inputs = {
    # Nixpkgs
    #    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-23.11-darwin";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    catppuccin.url = "github:catppuccin/nix";

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

    # nur: nix User Repository
    nur.url = "github:nix-community/NUR";
    nur.inputs.nixpkgs.follows = "nixpkgs";

    shell-flake.url = "path:./modules/flakes/shell";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    LazyVim = {
      url = "github:matadaniel/LazyVim-module";
      inputs.nixpkgs.follows = "nixpkgs";
   };

    nix-search-tv.url = "github:3timeslazy/nix-search-tv";
    sops-nix.url = "github:Mic92/sops-nix";
  };

  outputs = inputs@{ self, nixpkgs, catppuccin,nur, flake-parts, sops-nix, home-manager, nix-darwin, nix-search-tv,... }:
     flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "aarch64-darwin" ];

      perSystem = { system, pkgs, ... }: { };

      flake = {
        # --- nix-darwin Configuration ---
        darwinConfigurations = {
          "Leos-MacBook-Pro" = nix-darwin.lib.darwinSystem {
            system = "aarch64-darwin";
            modules = [
              ({ ... }: {
                # Let Determinate Nix handle Nix configuration
                nix.enable = false;
              })
              sops-nix.darwinModules.sops
              ./modules/darwin/default.nix
              home-manager.darwinModules.home-manager
              {
                # Home-manager configuration as a nix-darwin module
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.sharedModules = [ catppuccin.homeModules.catppuccin ];
                home-manager.users.ldangelo = import ./modules/home-manager/default.nix;
              }
            ];
            specialArgs = { inherit inputs; };
          };
        };

        # --- Home Manager Standalone Config (optional) ---
        homeConfigurations = {
          "ldangelo" = home-manager.lib.homeManagerConfiguration {
            pkgs = import nixpkgs;
            modules = [
              catppuccin.homeModules.catppuccin
              ./modules/home-manager/default.nix
              ./overlays
            ];
            home.username = "ldangelo";
            home.homeDirectory = "/Users/ldangelo";
          };
        };
      };
    };
    }

