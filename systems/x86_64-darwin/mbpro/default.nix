{
  lib,
  pkgs,
  namespace,
  ...
}:
with lib.${namespace}; {
  ofthenagels = {
    suites = {
      common = enabled;
      development = enabled;
    };

    desktop.yabai = enabled;
  };

  environment.systemPath = [
    "/opt/homebrew/bin"
  ];

  system.stateVersion = 4;
}
