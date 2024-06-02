{ lib, pkgs, namespace, ... }:
with lib;
with lib.${namespace}; {
  oftheangels = {

    security = { sops = enabled; };

    #    virtualization = { podman = enabled; };
    suites = {
      common = enabled;
      development = enabled;
    };

    #    desktop.yabai = enabled;
  };

  #  environment.systemPath = [ "/opt/homebrew/bin" ];

  system.stateVersion = 4;
}
