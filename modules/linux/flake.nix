{
  description = "Leo's Linux Nix Configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    catppuccin.url = "github:catppuccin/nix";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    flake-parts.url = "github:hercules-ci/flake-parts";
    nur.url = "github:nix-community/NUR";
    nur.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, catppuccin, nur, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" ];

      flake = {
        homeConfigurations = {
          "ldangelo-gx10" = home-manager.lib.homeManagerConfiguration {
            pkgs = import nixpkgs {
              system = "x86_64-linux";
              config.allowUnfree = true;
              overlays = [
                (self: super: {
                  # Add any custom overlays here
                })
              ];
            };
            modules = [
              catppuccin.homeModules.catppuccin
              ./modules/linux/home-manager/default.nix
              ../home-manager/pi-agent.nix
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
                pi-agent.settings = {
                  lastChangelogVersion = "0.72.1";
                  defaultProvider = "litellm";
                  defaultModel = "coding";
                  defaultThinkingLevel = "medium";
                  packages = [
                    "npm:pi-powerline-footer"
                    "npm:pi-mcp-adapter"
                    {
                      source = "${ensemblePi}/packages/pi";
                      # Ensemble currently ships an ask_user extension. We provide
                      # a Pi-version-compatible ask_user extension separately to
                      # avoid duplicate/conflicting tool registration.
                      extensions = [];
                    }
                  ];
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
                pi-agent.packages = [
                  "npm:pi-powerline-footer"
                  "npm:pi-mcp-adapter"
                ];
                pi-agent.mcpConfig = {
                  settings = {
                    toolPrefix = "server";
                    idleTimeout = 10;
                    directTools = false;
                  };
                  mcpServers = {};
                };
              })
            ];
          };
        };
      };
    };
}
