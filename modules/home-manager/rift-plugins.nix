{ config, lib, pkgs, ... }:
let
  # Rift companion plugins (https://acsandmann.github.io/rift-docs/ecosystem/plugins/).
  # None are in nixpkgs, so they are built from source. Each links rift-client /
  # rift-protocol via git dependencies, hence allowBuiltinFetchGit. stackline and
  # the highlighter link Apple's private SkyLight framework (build.rs), which the
  # darwin SDK stub provides.
  #
  # Rift upgrades: the IPC wire format is version-sensitive. After bumping rift
  # (homebrew), re-check each plugin's pinned rift rev and bump `rev` below.

  mkRiftPlugin = { pname, owner, repo, rev, hash }:
    let
      src = pkgs.fetchFromGitHub { inherit owner repo rev hash; };
    in
    pkgs.rustPlatform.buildRustPackage {
      inherit pname src;
      version = "0.1.0-${builtins.substring 0 7 rev}";
      cargoLock = {
        lockFile = "${src}/Cargo.lock";
        allowBuiltinFetchGit = true;
      };
      doCheck = false;
    };

  rift-container-highlighter = mkRiftPlugin {
    pname = "rift-container-highlighter";
    owner = "ubuntudroid";
    repo = "rift-container-highlighter";
    rev = "6c7f3975b799d8029f600f0ea2ec0e412932d530";
    hash = "sha256-I93CisaPkdhgDlhY6/KcFVAd4rzlB7bISRZWz8p3WK8=";
  };

  # Repo is named rift-companian; the executable is rift-app-indicator.
  rift-app-indicator = mkRiftPlugin {
    pname = "rift-app-indicator";
    owner = "Chandraprakash-Darji";
    repo = "rift-companian";
    rev = "878d4651158253a54aa246d957f9137d50c9f122";
    hash = "sha256-3d50iPwuS57MbxTs7tY55DJWI8y3zjOa7k7pNZUKVDw=";
  };

  stackline = mkRiftPlugin {
    pname = "stackline";
    owner = "acsandmann";
    repo = "stackline";
    rev = "2502c581739a24102163abae8448fad3f4b36317";
    hash = "sha256-EqVapFlImaWoHq3t+ul04aNjrxWSX2rXvU4BobMIfjo=";
  };

  mkAgent = { label, bin, logName }: {
    enable = true;
    config = {
      Label = label;
      ProgramArguments = [ bin ];
      RunAtLoad = true;
      KeepAlive = {
        SuccessfulExit = false;
        Crashed = true;
      };
      StandardOutPath = "/tmp/${logName}.out.log";
      StandardErrorPath = "/tmp/${logName}.err.log";
      ProcessType = "Interactive";
      LimitLoadToSessionType = "Aqua";
    };
  };

  # rift itself and rift-cli come from homebrew (modules/darwin/homebrew.nix).
  riftCli = "/opt/homebrew/bin/rift-cli";
  highlighter = "${rift-container-highlighter}/bin/rift-container-highlighter";
in
{
  home.packages = [ rift-container-highlighter rift-app-indicator stackline ];

  launchd.agents = {
    rift-app-indicator = mkAgent {
      label = "com.rift.app-indicator";
      bin = "${rift-app-indicator}/bin/rift-app-indicator";
      logName = "rift_app_indicator_${config.home.username}";
    };
    rift-stackline = mkAgent {
      label = "com.rift.stackline";
      bin = "${stackline}/bin/stackline";
      logName = "stackline_${config.home.username}";
    };
  };

  # stackline reads ~/.config/stackline/config.toml
  xdg.configFile."stackline/config.toml".source = ../../dotfiles/config/stackline/config.toml;

  # The rift config embeds the highlighter's store path (exec binding and the
  # selection_changed subscription), so it is templated instead of linked as-is.
  xdg.configFile."rift/config.toml".text =
    builtins.replaceStrings
      [ "@rift-container-highlighter@" "@rift-cli@" ]
      [ highlighter riftCli ]
      (builtins.readFile ../../dotfiles/config/rift/config.toml);
}
