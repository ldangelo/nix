{
  options,
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.virtualization.podman;
in {
  options.${namespace}.virtualization.podman = with types; {
    enable = mkBoolOpt false "Whether or not to enable Podman.";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [podman-compose];

    oftheangels.home.extraOptions = {
      home.shellAliases = {"docker-compose" = "podman-compose";};
    };

    # NixOS 22.05 moved NixOS Containers to a new state directory and the old
    # directory is taken over by OCI Containers (eg. podman). For systems with
    # system.stateVersion < 22.05, it is not possible to have both enabled.
    # This option disables NixOS Containers, leaving OCI Containers available.
#    boot.enableContainers = false;

    virtualization = {
      podman = {
        enable = cfg.enable;
        dockerCompat = true;
      };
    };
  };
}