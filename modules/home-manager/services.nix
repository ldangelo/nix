{ config, pkgs, lib, ... }:

{
  # Karabiner-DriverKit-VirtualHIDDevice system daemons
  # These are required for kanata to work on macOS
  #
  # IMPORTANT: Install Karabiner-DriverKit-VirtualHIDDevice first:
  #   brew install --cask karabiner-driverkit-virtualhiddevice
  # OR download from: https://github.com/pqrs-org/Karabiner-DriverKit-VirtualHIDDevice/releases
  #
  # After installation:
  # 1. Allow the system extension in System Settings → Privacy & Security
  # 2. Grant Input Monitoring permission to kanata
  # 3. Reboot may be required for the driver to activate
  #
  # Reference: https://github.com/jtroo/kanata/discussions/1537

  # Karabiner VirtualHIDDevice Daemon
  # Manages the virtual HID device driver
  launchd.daemons.karabiner-vhiddaemon = {
    serviceConfig = {
      Label = "com.ldangelo.karabiner-vhiddaemon";
      ProgramArguments = [
        "/Library/Application Support/org.pqrs/Karabiner-DriverKit-VirtualHIDDevice/Applications/Karabiner-VirtualHIDDevice-Daemon.app/Contents/MacOS/Karabiner-VirtualHIDDevice-Daemon"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardOutPath = "/var/log/karabiner-vhiddaemon.out.log";
      StandardErrorPath = "/var/log/karabiner-vhiddaemon.err.log";
    };
  };

  # Karabiner VirtualHIDDevice Manager
  # Activates the virtual HID device
  launchd.daemons.karabiner-vhidmanager = {
    serviceConfig = {
      Label = "com.ldangelo.karabiner-vhidmanager";
      ProgramArguments = [
        "/Applications/.Karabiner-VirtualHIDDevice-Manager.app/Contents/MacOS/Karabiner-VirtualHIDDevice-Manager"
        "activate"
      ];
      RunAtLoad = true;
      StandardOutPath = "/var/log/karabiner-vhidmanager.out.log";
      StandardErrorPath = "/var/log/karabiner-vhidmanager.err.log";
    };
  };

  # Kanata - Advanced keyboard remapper
  # https://github.com/jtroo/kanata
  launchd.agents.kanata = {
    enable = true;
    config = {
      ProgramArguments = [
        "${pkgs.kanata}/bin/kanata"
        "--cfg"
        "${config.home.homeDirectory}/.config/kanata/home-row.kbd"
      ];

      # Keep the service running
      KeepAlive = true;

      # Run on load
      RunAtLoad = true;

      # Standard output and error logs
      StandardOutPath = "${config.home.homeDirectory}/Library/Logs/kanata.out.log";
      StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/kanata.err.log";

      # Process type - interactive for keyboard input
      ProcessType = "Interactive";

      # Nice value for process priority
      Nice = -20;
    };
  };
}
