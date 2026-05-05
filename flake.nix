{
  description = "Leo's Nix Configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    catppuccin.url = "github:catppuccin/nix";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nix-darwin.url = "github:lnl7/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    flake-parts.url = "github:hercules-ci/flake-parts";
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

  outputs = inputs@{ self, nixpkgs, catppuccin, nur, flake-parts, sops-nix, home-manager, nix-darwin, nix-search-tv, ... }:
    let
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

        # ast-grep: skip failing test_scan_invalid_rule_id (Illegal byte sequence in sandbox)
        ast-grep = super.ast-grep.overrideAttrs (oldAttrs: {
          doCheck = false;
        });

        # Himalaya with OAuth2 support for Microsoft 365
        himalaya = super.himalaya.overrideAttrs (oldAttrs: {
          cargoBuildFeatures = (oldAttrs.cargoBuildFeatures or []) ++ [ "oauth2" ];
        });

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
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "aarch64-darwin" "aarch64-linux" ];

      flake = let
        darwinModules = [
          ({ ... }: {
            nix.enable = false;
          })
          sops-nix.darwinModules.sops
          ./modules/darwin/default.nix
          home-manager.darwinModules.home-manager
          {
            nixpkgs.overlays = common-overlays;
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "bak";
            home-manager.sharedModules = [
              catppuccin.homeModules.catppuccin
              ./modules/home-manager/pi-agent.nix
            ];
            home-manager.users.ldangelo = import ./modules/home-manager/default.nix;
          }
        ];
      in {
        darwinConfigurations = {
          "Leos-MacBook-Pro" = nix-darwin.lib.darwinSystem {
            system = "aarch64-darwin";
            modules = darwinModules;
            specialArgs = { inherit inputs; isWorkstation = true; };
          };

          "Leos-Mac-mini" = nix-darwin.lib.darwinSystem {
            system = "aarch64-darwin";
            modules = darwinModules;
            specialArgs = { inherit inputs; isWorkstation = false; };
          };
        };

        homeConfigurations = {
          "ldangelo" = home-manager.lib.homeManagerConfiguration {
            pkgs = import nixpkgs {
              system = "aarch64-darwin";
              config.allowUnfree = true;
              overlays = common-overlays;
            };
            modules = [
              catppuccin.homeModules.catppuccin
              ./modules/home-manager/default.nix
              ./overlays
              {
                home.username = "ldangelo";
                home.homeDirectory = "/Users/ldangelo";
              }
            ];
          };

          "ldangelo-linux" = home-manager.lib.homeManagerConfiguration {
            pkgs = import nixpkgs {
              system = "aarch64-linux";
              config.allowUnfree = true;
              overlays = common-overlays;
            };
            modules = [
              catppuccin.homeModules.catppuccin
              ./modules/linux/home-manager/default.nix
              ./modules/home-manager/pi-agent.nix
              ({ pkgs, ... }: {
                pi-agent.enable = true;
                pi-agent.settings = {
                  lastChangelogVersion = "0.72.1";
                  defaultProvider = "litellm";
                  defaultModel = "coding";
                  defaultThinkingLevel = "medium";
                  packages = [ "npm:pi-powerline-footer" ];
                  powerline = {
                    preset = "nerd";
                  };
                  workingVibeMode = "file";
                  workingVibe = "off";
                  bashMode = {
                    toggleShortcut = "ctrl+shift+b";
                    transcriptMaxLines = 2000;
                    transcriptMaxBytes = 524288;
                  };
                };
                pi-agent.binTools = with pkgs; [
                  fd
                  ripgrep
                ];
                pi-agent.packages = [ "npm:pi-powerline-footer" ];
              })
            ];
          };
        };
      };
    };
}
