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

    # Local patched fork of hiroppy/tmux-agent-sidebar (adds OMP agent support).
    # Tracked as a flake input instead of `builtins.path { path = /abs; }`
    # because darwin-rebuild evaluates in pure mode and forbids absolute paths.
    tmux-agent-sidebar-src = {
      url = "path:/Users/ldangelo/tmux-agent-sidebar";
      flake = false;
    };

  };

  outputs = inputs@{ self, nixpkgs, catppuccin, nur, flake-parts, sops-nix, home-manager, nix-darwin, nix-search-tv, tmux-agent-sidebar-src, ... }:
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

        # pipx: skip test_package_specifier failures (newer packaging lib changed @-spacing in output)
        pipx = super.pipx.overridePythonAttrs (oldAttrs: {
          doCheck = false;
          doInstallCheck = false;
        });

        # Avoid GNU Emacs GUI build failure on current unstable/Xcode.
        # Mail tools only need notmuch CLI and batch Emacs for mu4e byte-compilation.
        notmuch = super.notmuch.override { withEmacs = false; };
        mu = super.mu.override { emacs = super.emacs-nox; };

        # bun 1.3.14 — darwin-only binary override. Keep Linux on nixpkgs' native bun;
        # using the darwin zip on Linux causes Exec format errors during deploy.
        bun = if super.stdenv.hostPlatform.isDarwin && super.stdenv.hostPlatform.isAarch64
          then super.bun.overrideAttrs (_: {
            version = "1.3.14";
            src = super.fetchurl {
              url = "https://github.com/oven-sh/bun/releases/download/bun-v1.3.14/bun-darwin-aarch64.zip";
              hash = "sha256-2LliIYKK1vl6x6wKt+lYcjQa92MAHogD6CZ2UsJlJiA=";
            };
          })
          else super.bun;

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
            # nix-darwin manual currently passes removed --toc-depth to the
            # latest nixos-render-docs from unstable. Disable generated docs
            # until nix-darwin catches up.
            documentation.enable = false;
            # Current darwin-uninstaller derivation evaluates its own default
            # system with docs enabled, which trips the same renderer breakage.
            system.tools.darwin-uninstaller.enable = false;
          })
          sops-nix.darwinModules.sops
          ./modules/darwin/default.nix
          home-manager.darwinModules.home-manager
          {
            nixpkgs.overlays = common-overlays;
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "bak";
            home-manager.extraSpecialArgs = { inherit inputs; };
            home-manager.sharedModules = [
              sops-nix.homeManagerModules.sops
            ];
            home-manager.users.ldangelo = { pkgs, ... }: {
              imports = [
                catppuccin.homeModules.catppuccin
                ./modules/home-manager/default.nix
              ];
              # Parallel AI-agent worktree workflow (worktrunk + clash + tmuxinator).
              # NOTE: requires versions + hashes filled in ai-worktrees.nix before deploy.
              # aiWorktrees.enable = true;  # disabled: ai-worktrees.nix has fakeHash placeholders (worktrunk + clash-sh). Re-enable after pasting real versions + hashes per the file's own onboarding steps.
            };
          }
        ];
      in {
        darwinConfigurations = {
          "Leos-MacBook-Pro" = nix-darwin.lib.darwinSystem {
            system = "aarch64-darwin";
            modules = darwinModules;
            specialArgs = { inherit inputs; isWorkstation = true; };
          };

          "Sunstone-MacBook-Pro" = nix-darwin.lib.darwinSystem {
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
            extraSpecialArgs = { inherit inputs; };
            modules = [
              catppuccin.homeModules.catppuccin
              sops-nix.homeManagerModules.sops
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
            extraSpecialArgs = { inherit inputs; };
            modules = [
              catppuccin.homeModules.catppuccin
              sops-nix.homeManagerModules.sops
              ./modules/linux/home-manager/default.nix
            ];
          };
        };
      };
    };
}
