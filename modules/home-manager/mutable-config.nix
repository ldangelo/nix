# Mutable config files
#
# For XDG config files that the program itself rewrites (or locks next to
# the file via the resolved path, which lands in read-only /nix/store when the
# file is a home-manager symlink). Nix still declares the content; this module
# only changes how it is delivered:
#
#   - the home-manager symlink is disabled
#   - at activation, a store symlink is replaced by a real, writable copy
#   - an existing real file is left alone (runtime edits survive deploys)
#
# To re-seed from Nix, delete the file and deploy.
{ config, lib, pkgs, ... }:

let
  cfg = config.mutableConfig;
  home = config.home.homeDirectory;
  xdgDir = config.xdg.configHome;
in {
  options.mutableConfig.xdgFiles = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    description = "xdg.configFile names to deliver as real files instead of store symlinks";
  };

  config = lib.mkIf (cfg.xdgFiles != [ ]) {
    xdg.configFile = lib.genAttrs cfg.xdgFiles (_: {
      enable = lib.mkForce false;
    });

    home.activation.materializeMutableConfigs = lib.hm.dag.entryAfter [ "linkGeneration" ] (
      lib.concatMapStrings (name: ''
        dest=${lib.escapeShellArg "${xdgDir}/${name}"}
        if [ -L "$dest" ] || [ ! -e "$dest" ]; then
          rm -f "$dest"
          mkdir -p "$(dirname "$dest")"
          ${pkgs.coreutils}/bin/install -m 644 ${config.xdg.configFile.${name}.source} "$dest"
        fi
      '') cfg.xdgFiles
    );
  };
}
