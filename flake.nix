{
  description = "Oftheangels nix config";

  inputs = {
    catppuccin.url = "github:catppuccin/nix";

    # macOS Support (master)
    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # System Deployment
    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # GPG default configuration
    gpg-base-conf = {
      url = "github:drduh/config";
      flake = false;
    };

    # Home Manager (master)
    home-manager = {
      url = "github:nix-community/home-manager";
      # url = "git+file:///home/khaneliman/Documents/github/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Weekly updating nix-index database
    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # NixPkgs (nixos-unstable)
    nixpkgs = { url = "github:nixos/nixpkgs/nixos-unstable"; };

    # NixPkgs-Wayland
    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # NixOS WSL Support
    nixos-wsl = {
      url = "github:nix-community/nixos-wsl";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Run unpatched dynamically compiled binaries
    nix-ld-rs = {
      url = "github:nix-community/nix-ld-rs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Neovim nix configuration
    nixvim = {
      url = "github:nix-community/nixvim";
      # url = "git+file:///Users/khaneliman/Documents/github/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Nix User Repository (master)
    nur = { url = "github:nix-community/NUR"; };

    pre-commit-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";

    # Snowfall Lib
    snowfall-lib = {
      url = "github:snowfallorg/lib";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Snowfall Flake
    snowfall-flake = {
      url = "github:snowfallorg/flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Sops (Secrets)
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Spicetify
    spicetify-nix = {
      url = "github:Gerg-L/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    tmux.url = "github:jakehamilton/tmux";
    tmux.inputs = { nixpkgs.follows = "nixpkgs"; };
    # Yubikey Guide
    yubikey-guide = {
      url = "github:drduh/YubiKey-Guide";
      flake = false;
    };
  };

  outputs = inputs:
    let
      lib = inputs.snowfall-lib.mkLib {
        inherit inputs;
        src = ./snowfall;

        snowfall = {
          meta = {
            name = "oftheangels";
            title = "Oftheangels Nix Config";
          };

          namespace = "oftheangels";
        };
      };
    in lib.mkFlake {
      channels-config = {
        allowUnfree = true;
        permittedInsecurePackages = [ "electron-25.9.0" ];
      };

      overlays = with inputs;
        [
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

              nativeBuildInputs = [
                super.pkg-config
                super.automake
                super.autoconf
                super.libtool
              ];
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
          #        avalanche.overlays.default
          #        aux-website.overlays.default
          #        neovim.overlays.default
          #                tmux.overlay
          #        flake.overlays.default
          #        thaw.overlays.default
          #        drift.overlays.default
          #        icehouse.overlays.default
          #        rf.overlays.default
          #        attic.overlays.default
          # snowfall-docs.overlays.default
          #        nixpkgs-news.overlays.default
        ];

      homes.modules = with inputs; [
        catppuccin.homeManagerModules.catppuccin
        #        hypr-socket-watch.homeManagerModules.default
        nix-index-database.hmModules.nix-index
        nixvim.homeManagerModules.nixvim
        sops-nix.homeManagerModules.sops
        spicetify-nix.homeManagerModules.default
      ];

      systems = {
        modules = {
          darwin = with inputs; [ nixvim.nixDarwinModules.nixvim ];

          nixos = with inputs; [
            # catppuccin.nixosModules.catppuccin
            #            lanzaboote.nixosModules.lanzaboote
            nixvim.nixosModules.nixvim
            sops-nix.nixosModules.sops
          ];
        };
      };

      deploy = lib.mkDeploy { inherit (inputs) self; };

      checks = builtins.mapAttrs
        (system: deploy-lib: deploy-lib.deployChecks inputs.self.deploy)
        inputs.deploy-rs.lib;
    };
}
