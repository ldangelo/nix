{
  description = "Oftheangels nix config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";


    # NixPkgs Unstable (nixos-unstable)
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    # Home Manager (release-22.05)
    home-manager.url = "github:nix-community/home-manager/release-23.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # macOS Support (master)
    darwin.url = "github:lnl7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    # Hardware Configuration
    nixos-hardware.url = "github:nixos/nixos-hardware";

    # Generate System Images
    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-generators.inputs.nixpkgs.follows = "nixpkgs";

    # Snowfall Lib
    snowfall-lib.url = "github:snowfallorg/lib?ref=v3.0.0";
    snowfall-lib.inputs.nixpkgs.follows = "nixpkgs";

    # Avalanche
    avalanche.url = "github:snowfallorg/avalanche";
    # avalanche.url = "path:/home/short/work/@snowfallorg/avalanche";
    avalanche.inputs.nixpkgs.follows = "unstable";

    aux-website.url = "github:auxolotl/website";
    aux-website.inputs.nixpkgs.follows = "nixpkgs";

    # Snowfall Flake
    flake.url = "github:snowfallorg/flake?ref=v1.4.0";
    flake.inputs.nixpkgs.follows = "unstable";

    # Snowfall Thaw
    thaw.url = "github:snowfallorg/thaw?ref=v1.0.6";

    # Snowfall Drift
    drift.url = "github:snowfallorg/drift";
    drift.inputs.nixpkgs.follows = "nixpkgs";

    # Comma
    comma.url = "github:nix-community/comma";
    comma.inputs.nixpkgs.follows = "unstable";

    # System Deployment
    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.nixpkgs.follows = "nixpkgs";

    # Run unpatched dynamically compiled binaries
    nix-ld.url = "github:Mic92/nix-ld";
    nix-ld.inputs.nixpkgs.follows = "unstable";

    # Neovim
    neovim.url = "github:jakehamilton/neovim";
    neovim.inputs.nixpkgs.follows = "unstable";

    # Tmux
    tmux.url = "github:jakehamilton/tmux";
    tmux.inputs = {
      nixpkgs.follows = "nixpkgs";
      unstable.follows = "unstable";
    };

    # Binary Cache
    attic = {
      url = "github:zhaofengli/attic";

      # FIXME: A specific version of Rust is needed right now or
      # the build fails. Re-enable this after some time has passed.
      inputs.nixpkgs.follows = "unstable";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };

    # Vault Integration
    vault-service = {
      url = "github:DeterminateSystems/nixos-vault-service";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Flake Hygiene
    flake-checker = {
      url = "github:DeterminateSystems/flake-checker";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Discord Replugged
    replugged.url = "github:LunNova/replugged-nix-flake";
    replugged.inputs.nixpkgs.follows = "unstable";

    # Discord Replugged plugins / themes
    discord-tweaks = {
      url = "github:NurMarvin/discord-tweaks";
      flake = false;
    };
    discord-nord-theme = {
      url = "github:DapperCore/NordCord";
      flake = false;
    };

    # Backup management
    icehouse = {
      url = "github:snowfallorg/icehouse?ref=v1.1.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Yubikey Guide
    yubikey-guide = {
      url = "github:drduh/YubiKey-Guide";
      flake = false;
    };

    # GPG default configuration
    gpg-base-conf = {
      url = "github:drduh/config";
      flake = false;
    };

    bibata-cursors = {
      url = "github:suchipi/Bibata_Cursor";
      flake = false;
    };

    rf = {
      url = "github:jakehamilton/rf";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Hosted Sites
    lasersandfeelings = {
      url = "github:jakehamilton/lasersandfeelings";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.unstable.follows = "unstable";
    };
    pungeonquest = {
      url = "github:jakehamilton/pungeonquest";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.unstable.follows = "unstable";
    };
    scrumfish = {
      url = "github:jakehamilton/scrumfi.sh";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.unstable.follows = "unstable";
    };
    retrospectacle = {
      url = "github:jakehamilton/retrospectacle.app";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.unstable.follows = "unstable";
    };
    jakehamilton-website = {
      url = "github:jakehamilton/jakehamilton.dev";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.unstable.follows = "unstable";
    };
    noop-ai-website = {
      url = "github:noopai/noop.ai";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.unstable.follows = "unstable";
    };
    sokoban-app-website = {
      url = "https://github.com/jakehamilton/sokoban.app/releases/download/v1/sokoban.app.tar.gz";
      flake = false;
    };
    snowfall-docs = {
      url = "github:snowfallorg/docs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs-news = {
      url = "github:jakehamilton/nixpkgs.news";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs: let
    lib = inputs.snowfall-lib.mkLib {
      inherit inputs;
      src = ./.;

      snowfall = {
        meta = {
          name = "oftheangels";
          title = "Oftheangels Nix Config";
        };

        namespace = "oftheangels";
      };
    };
  in
    lib.mkFlake {
      channels-config = {
        allowUnfree = true;
        permittedInsecurePackages = [
          "electron-25.9.0"
        ];
      };

      overlays = with inputs; [
        avalanche.overlays.default
        aux-website.overlays.default
        neovim.overlays.default
        tmux.overlay
        flake.overlays.default
        thaw.overlays.default
        drift.overlays.default
        icehouse.overlays.default
        rf.overlays.default
        attic.overlays.default
        snowfall-docs.overlays.default
        nixpkgs-news.overlays.default
      ];

      systems.modules.nixos = with inputs; [
        avalanche.nixosModules."avalanche/desktop"
        home-manager.nixosModules.home-manager
        nix-ld.nixosModules.nix-ld
        vault-service.nixosModules.nixos-vault-service
        # TODO: Replace oftheangels.services.attic now that vault-agent
        # exists and can force override environment files.
        # attic.nixosModules.atticd
      ];

      deploy = lib.mkDeploy {inherit (inputs) self;};

      checks =
        builtins.mapAttrs
        (system: deploy-lib:
          deploy-lib.deployChecks inputs.self.deploy)
        inputs.deploy-rs.lib;
    };
}
# {
#   description = "Leo's Nix Configuration";

#   inputs = {
#     # Nixpkgs
#     #    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-23.11-darwin";
#     nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

#     #    flake-utils.url = "github:numtide/flake-utils";


#     # Darwin
#     nix-darwin.url = "github:lnl7/nix-darwin";
#     nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

#     # Home manager
#     home-manager.url = "github:nix-community/home-manager";
#     home-manager.inputs.nixpkgs.follows = "nixpkgs";
#     # nixvim
#     #    nixvim.url = "github:nix-community/nixvim";
#     #    nixvim.inputs.nixpkgs.follows = "nixpkgs";

#     # flake-parts
#     flake-parts.url = "github:hercules-ci/flake-parts";
#     flake-parts.inputs.nixpkgs.follows = "nixpkgs";

#     # nur: nix User Repository
#     nur.url = "github:nix-community/NUR";
#     nur.inputs.nixpkgs.follows = "nixpkgs";

#     stylix.url = "github:danth/stylix";
#     stylix.inputs.nixpkgs.follows = "nixpkgs";

#     # Overlays
#     sketchybar-lua = {
#       url = "github:FelixKratz/SbarLua";
#       flake = false;
#     };

#     emacs-overlay = {
#       url = "github:nix-community/emacs-overlay";
#       inputs.nixpkgs.follows = "nixpkgs";
#     };
#   };

#   outputs = inputs@{ self, nixpkgs, nur, home-manager, nix-darwin, stylix,... }:
#     let
#       common-overlays = [

#       (self: super: {
#         cyrus-sasl-xoauth2 = super.pkgs.stdenv.mkDerivation rec {
#           pname = "cyrus-sasl-xoauth2";
#           version = "master";

#           src = super.pkgs.fetchFromGitHub {
#             owner = "moriyoshi";
#             repo = "cyrus-sasl-xoauth2";
#             rev = "master";
#             sha256 = "sha256-OlmHuME9idC0fWMzT4kY+YQ43GGch53snDq3w5v/cgk=";
#           };

#           nativeBuildInputs =
#             [ super.pkg-config super.automake super.autoconf super.libtool ];
#           propagatedBuildInputs = [ super.cyrus_sasl ];

#           buildPhase = ''
#             ./autogen.sh
#             ./configure
#           '';

#           installPhase = ''
#             make DESTDIR="$out" install
#           '';

#           meta = with super.pkgs.lib; {
#             homepage = "https://github.com/moriyoshi/cyrus-sasl-xoauth2";
#             description = "XOAUTH2 mechanism plugin for cyrus-sasl";
#           };
#         };

#         # https://github.com/NixOS/nixpkgs/issues/108480#issuecomment-1115108802
#         isync-oauth2 = super.buildEnv {
#           name = "isync-oauth2";
#           paths = [ super.isync ];
#           pathsToLink = [ "/bin" ];
#           nativeBuildInputs = [ super.makeWrapper ];
#           postBuild = ''
#             wrapProgram "$out/bin/mbsync" \
#               --prefix SASL_PATH : "${super.cyrus_sasl.out.outPath}/lib/sasl2:${self.cyrus-sasl-xoauth2}/usr/lib/sasl2"
#           '';
#         };


#       })
#       ];

#       darwinConfiguration = { pkgs, ... }: {
#         environment.systemPackages = [ pkgs.vim ];

#         services.nix-daemon.enable = true;

#         nix.settings.experimental-features = "nix-command flakes";

#         programs.zsh.enable = true;

#         system.configurationRevision = self.rev or self.dirtyRev or null;

#         system.stateVersion = 4;

#         nixpkgs.hostPlatform = "x86_64-darwin";
#         nixpkgs.config.allowUnfree = true;
#         nixpkgs.overlays = common-overlays;
#       };

#     in {
#       darwinConfigurations."ldangelo" = nix-darwin.lib.darwinSystem {
#         modules = [
# #          stylix.darwinModules.stylix
#           darwinConfiguration
#           ./modules/darwin
#           home-manager.darwinModules.home-manager
#           {
#             home-manager = {
#               useGlobalPkgs = true;
#               useUserPackages = true;
#               backupFileExtension = "backup";
#               users.ldangelo.imports = [
#                 stylix.homeManagerModules.stylix
#                 ./modules/home-manager
#                 ./overlays
#               ];
#             };
#           }
#         ];
#       };

#       darwinPackages = self.darwinConfigurations."ldangelo".pkgs;
#     };
# }
