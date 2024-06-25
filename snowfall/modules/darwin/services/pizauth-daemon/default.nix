{ config, lib, pkgs, ... }:
with lib;

let cfg = config.services.pizauth-daemon;
in {
  options = {
    services.pizauth-daemon.enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to enable the pizauth-daemon service.";
    };

    services.pizauth-daemon.enableSocketListener = mkOption {
      type = types.bool;
      default = false;
      description = "";
    };

    services.pizauth-daemon.logFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      example = "/tmp/pizauth-daemon.log";
      description = ''
        The logfile to use for the pizauth-daemon service. Alternatively
        {command}`sudo launchctl debug system/org.nixos.pizauth-daemon --stderr`
        can be used to stream the logs to a shell after restarting the service with
        {command}`sudo launchctl kickstart -k system/org.nixos.pizauth-daemon`.
      '';
    };

    services.pizauth-daemon.homeDir = mkOption {
      type = types.nullOr types.path;
      default = "/Users/ldangelo/";
      description = "The HOME to use for pizauth-daemon.";
    };
    services.pizauth-daemon.cacheDir = mkOption {
      type = types.nullOr types.path;
      default = "/Users/ldangelo/.local/share";
      description = "The XDG_DATA_HOME_DIR to use for pizauth-daemon.";
    };
    services.pizauth-daemon.tempDir = mkOption {
      type = types.nullOr types.path;
      default = "/tmp";
      description = "The TMPDIR to use for pizauth-daemon.";
    };
  };

  config = mkIf cfg.enable {
    launchd.user.agents.pizauth-daemon = {
      serviceConfig.ProgramArguments = [
        "/usr/local/bin/pizauth"
        "server"
        "-c"
        "/Users/ldangelo/.config/pizauth.conf"
        "-d"
        "-vvvvv"
      ];
      serviceConfig.ProcessType = config.nix.daemonProcessType;
      serviceConfig.Label = "pizauth.daemon";
      serviceConfig.StandardErrorPath = cfg.logFile;

      serviceConfig.KeepAlive = mkIf (!cfg.enableSocketListener) true;

      serviceConfig.EnvironmentVariables = mkMerge [
        config.nix.envVars
        {
          HOME = mkIf (cfg.homeDir != null) cfg.homeDir;
          TMPDIR = mkIf (cfg.tempDir != null) cfg.tempDir;
          XDG_DATA_HOME = mkIf (cfg.cacheDir != null) cfg.cacheDir;
        }
      ];
    };
  };
}
