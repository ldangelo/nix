{ config, lib, pkgs, ... }:
with lib;

let cfg = config.services.emacs-daemon;
in
{
options = {
    services.emacs-daemon.enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to enable the emacs-daemon service.";
    };

    services.emacs-daemon.enableSocketListener = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to make the nix-daemon service socket activated.";
    };

    services.emacs-daemon.logFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      example = "/var/log/emacs-daemon.log";
      description = ''
        The logfile to use for the emacs-daemon service. Alternatively
        {command}`sudo launchctl debug system/org.nixos.emacs-daemon --stderr`
        can be used to stream the logs to a shell after restarting the service with
        {command}`sudo launchctl kickstart -k system/org.nixos.emacs-daemon`.
      '';
    };

    services.emacs-daemon.tempDir = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "The TMPDIR to use for emacs-daemon.";
    };
  };


  config = mkIf cfg.enable {
    launchd.daemons.emacs-daemon = {
      serviceConfig.ProgramArguments = [
        "/Applications/Emacs.app/Contents/MacOS/Emacs"
        "--fg-daemon=work"
      ];
      serviceConfig.ProcessType = config.nix.daemonProcessType;
      serviceConfig.Label = "emacs.daemon";
      serviceConfig.StandardErrorPath = cfg.logFile;

      serviceConfig.KeepAlive = mkIf (!cfg.enableSocketListener) true;

    serviceConfig.EnvironmentVariables = mkMerge [
      config.nix.envVars
      {
        HOME = "/Users/ldangelo";
        TMPDIR = mkIf (cfg.tempDir != null) cfg.tempDir;

      }
    ];
     };
  };
}
