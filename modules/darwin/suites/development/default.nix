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
  cfg = config.${namespace}.suites.development;
in {
  options.${namespace}.suites.development = with types; {
    enable =
      mkBoolOpt false
      "Whether or not to enable common development configuration.";
  };

  config = mkIf cfg.enable {
    oftheangels = {
      apps = {
        vscode = enabled;
        emacs = enabled;
      };

      tools = {
        # at = enabled;
        direnv = enabled;
        # go = enabled;
        # http = enabled;
        # k8s = enabled;
#        node = enabled;
        # titan = enabled;
#        python = enabled;
#        java = enabled;
      };

      #      virtualization = { podman = enabled; };
    };
  };
}
