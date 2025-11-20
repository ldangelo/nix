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
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";

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

  outputs = inputs@{ self, nixpkgs, catppuccin,nur, home-manager, nix-darwin, nix-search-tv,... }:
    let
      username = "ldangelo";
      common-overlays = [

      (self: super: {
        cyrus-sasl-xoauth2 = super.pkgs.stdenv.mkDerivation rec {
          pname = "cyrus-sasl-xoauth2";
          version = "master";

          src = super.pkgs.fetchFromGitHub {
            owner = "moriyoshi";
            repo = "cyrus-sasl-xoauth2";
            rev = "master";
            sha256 = "sha256-OlmHuME9idC0fWMzT4kY+YQ43GGch53snDq3w5v/cgk=";
          };

          nativeBuildInputs =
            [ super.pkg-config super.automake super.autoconf super.libtool ];
          propagatedBuildInputs = [ super.cyrus_sasl ];

          buildPhase = ''
            ./autogen.sh
            ./configure
          '';

          installPhase = ''
            make DESTDIR="$out" install
          '';

          meta = with super.pkgs.lib; {
            homepage = "https://github.com/moriyoshi/cyrus-sasl-xoauth2";
            description = "XOAUTH2 mechanism plugin for cyrus-sasl";
          };
        };

        # https://github.com/NixOS/nixpkgs/issues/108480#issuecomment-1115108802
        isync-oauth2 = super.buildEnv {
          name = "isync-oauth2";
          paths = [ super.isync ];
          pathsToLink = [ "/bin" ];
          nativeBuildInputs = [ super.makeWrapper ];
          postBuild = ''
            wrapProgram "$out/bin/mbsync" \
              --prefix SASL_PATH : "${super.cyrus_sasl.out.outPath}/lib/sasl2:${self.cyrus-sasl-xoauth2}/usr/lib/sasl2"
          '';
        };
      })
      ];

      configuration = { pkgs, ... }: {
#        environment.systemPackages = [ pkgs.vim ];
	system.primaryUser = "ldangelo";
	nix.enable = false;
        nix.settings.experimental-features = "nix-command flakes";

        programs.zsh.enable = true;

        system.configurationRevision = self.rev or self.dirtyRev or null;

        system.stateVersion = 4;

        nixpkgs.hostPlatform = "aarch64-darwin";
        nixpkgs.config.allowUnfree = true;
        nixpkgs.overlays = common-overlays;
      };

    in {
      flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-darwin" ];

      perSystem = { system, pkgs, ... }: { };

      flake = {
        # --- nix-darwin Configuration ---
        darwinConfigurations = {
          "my-mac" = nix-darwin.lib.darwinSystem {
            system = "x86_64-darwin";
            modules = [
              configuration
              sops-nix.darwinModules.sops
              ./modules/darwin/default.nix
              home-manager.darwinModules.home-manager
              {
                # Home-manager configuration as a nix-darwin module
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.ldangelo = import ./modules/home-manager/home.nix;
              }
            ];
            specialArgs = { inherit inputs; };
          };
        };

        # --- Home Manager Standalone Config (optional) ---
        homeConfigurations = {
          "ldangelo" = home-manager.lib.homeManagerConfiguration {
            pkgs = import nixpkgs { system = "x86_64-darwin"; };
            modules = [
              catppuccin.homeModules.catppuccin
              sops-nix.homeManagerModules.sops
              ./modules/home-manager/default.nix
              ./overlays
            ];
            home.username = "ldangelo";
            home.homeDirectory = "/Users/ldangelo";
          };
        };
      };
    };

     darwinPackages = self.darwinConfigurations."ldangelo".pkgs;
}
