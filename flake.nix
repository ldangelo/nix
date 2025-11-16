{
  description = "Leo's Nix Configuration";

  inputs = {
    # Nixpkgs
    #    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-23.11-darwin";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

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

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    LazyVim = {
      url = "github:matadaniel/LazyVim-module";
      inputs.nixpkgs.follows = "nixpkgs";
   };

    nix-search-tv.url = "github:3timeslazy/nix-search-tv";
  };

  outputs = inputs@{ self, nixpkgs, nur, home-manager, nix-darwin, nix-search-tv... }:
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
      darwinConfigurations."ldangelo" = nix-darwin.lib.darwinSystem {
        modules = [
         configuration

          ./modules/darwin
          home-manager.darwinModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              backupFileExtension = "backup";
	      extraSpecialArgs = {
	        inherit inputs;
	      };
              users.ldangelo.imports = [
                #                nixvim.homeManagerModules.nixvim
                ./modules/home-manager
                ./overlays
              ];
            };
          }
        ];
      };

      darwinPackages = self.darwinConfigurations."ldangelo".pkgs;
    };
}
