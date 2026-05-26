{
  description = "Leo's Nix Configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

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

    catppuccin.url = "github:catppuccin/nix";

  };

  outputs = inputs@{ self, nixpkgs, catppuccin, nur, flake-parts, sops-nix, home-manager, nix-darwin, nix-search-tv, ... }:
    let
      common-overlays = [
      (self: super: {
        # NVM - wrapper script that loads the nvm.sh from ~/.nvm
        nvm = super.writeShellScriptBin "nvm" ''
          export NVM_DIR="$HOME/.nvm"
          if [ ! -d "$NVM_DIR" ]; then
            mkdir -p "$NVM_DIR"
            git clone --depth 1 https://github.com/nvm-sh/nvm.git "$NVM_DIR" 2>/dev/null || true
          fi
          [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
          [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
          if [ "$1" = "nvm" ]; then
            shift
            exec "$0" "$@"
          fi
          exec "$@"
        '';

        # ast-grep: skip failing test_scan_invalid_rule_id (Illegal byte sequence in sandbox)
        ast-grep = super.ast-grep.overrideAttrs (oldAttrs: {
          doCheck = false;
        });

        # pipx 1.8.0 tests fail with newer `packaging` lib (pkg@url vs pkg @ url
        # normalization). Skip tests until upstream catches up.
        pipx = super.pipx.overridePythonAttrs (_: {
          doCheck = false;
          doInstallCheck = false;
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
              --prefix SASL_PATH : "${super.cyrus_sasl.out.outPath}/lib/sasl2"
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
              sops-nix.homeManagerModules.sops
              ./modules/home-manager/pi-agent.nix
              ./modules/home-manager/context-mode.nix
            ];
            home-manager.users.ldangelo = { pkgs, ... }: {
              imports = [
                catppuccin.homeModules.catppuccin
                ./modules/home-manager/default.nix
              ];
              context-mode.enable = true;
              pi-agent.enable = true;
              pi-agent.models = builtins.fromJSON (builtins.readFile ./pi-models.json);
              pi-agent.packages = [
                {
                  source = "${pkgs.fetchFromGitHub {
                    owner = "FortiumPartners";
                    repo = "ensemble";
                    rev = "faa88672815559b3739b8da5ec5c50607553eb5d";
                    hash = "sha256-0jQiNfIWLY0sQP0el6b1WgjvjfT6c9YC0hpzFChka5A=";
                  }}/packages/pi";
                  extensions = [];
                }
              ];
              pi-agent.binTools = with pkgs; [ fd ripgrep nodejs bun ];
            };
          }
        ];
      in {
        darwinConfigurations = {
          "Leos-MacBook-Pro" = nix-darwin.lib.darwinSystem {
            system = "aarch64-darwin";
            modules = darwinModules;
            specialArgs = { inherit inputs; isWorkstation = true; };
            # pi-agent config moved to home-manager only
          };

          "Sunstone-MacBook-Pro" = nix-darwin.lib.darwinSystem {
            system = "aarch64-darwin";
            modules = darwinModules;
            specialArgs = { inherit inputs; isWorkstation = true; };
            # pi-agent config moved to home-manager only
          };

          "Leos-Mac-mini" = nix-darwin.lib.darwinSystem {
            system = "aarch64-darwin";
            modules = darwinModules;
            specialArgs = { inherit inputs; isWorkstation = false; };
            # pi-agent config moved to home-manager only
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
              sops-nix.homeManagerModules.sops
              ./modules/home-manager/default.nix
              ./modules/home-manager/context-mode.nix
              ./overlays
              ({ pkgs, ... }:
              {
                context-mode.enable = true;
                pi-agent.enable = true;
                pi-agent.models = builtins.fromJSON (builtins.readFile ./pi-models.json);
                pi-agent.packages = [
                  {
                    source = "${pkgs.fetchFromGitHub {
                      owner = "FortiumPartners";
                      repo = "ensemble";
                      rev = "faa88672815559b3739b8da5ec5c50607553eb5d";
                      hash = "sha256-0jQiNfIWLY0sQP0el6b1WgjvjfT6c9YC0hpzFChka5A=";
                    }}/packages/pi";
                    extensions = [];
                  }
                ];
                pi-agent.binTools = with pkgs; [ fd ripgrep nodejs bun ];
              })
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
              sops-nix.homeManagerModules.sops
              ./modules/linux/home-manager/default.nix
              ({ pkgs, ... }:
              let
                ensemblePi = pkgs.fetchFromGitHub {
                  owner = "FortiumPartners";
                  repo = "ensemble";
                  rev = "faa88672815559b3739b8da5ec5c50607553eb5d";
                  hash = "sha256-0jQiNfIWLY0sQP0el6b1WgjvjfT6c9YC0hpzFChka5A=";
                };
              in {
                pi-agent.enable = true;
                pi-agent.models = builtins.fromJSON (builtins.readFile ./pi-models.json);
                pi-agent.packages = [
                  {
                    source = "${ensemblePi}/packages/pi";
                    extensions = [];
                  }
                ];
                pi-agent.binTools = with pkgs; [ fd ripgrep nodejs bun ];
              })
            ];
          };
        };
      };
    };
}
