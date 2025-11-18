{ config, pkgs, lib, ... }:

{
  # Launchd Services Configuration
  # System-level daemons and user-level agents for macOS services

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

  # Karabiner VirtualHIDDevice Daemon (system-level)
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

  # Karabiner VirtualHIDDevice Manager (system-level)
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

  # Kanata - Advanced keyboard remapper (user-level agent)
  # https://github.com/jtroo/kanata
  launchd.user.agents.kanata = {
    serviceConfig = {
      Label = "com.ldangelo.kanata";
      ProgramArguments = [
        "${pkgs.kanata}/bin/kanata"
        "--cfg"
        "${config.users.users.ldangelo.home}/.config/kanata/home-row.kbd"
      ];
      KeepAlive = true;
      RunAtLoad = true;
      StandardOutPath = "${config.users.users.ldangelo.home}/Library/Logs/kanata.out.log";
      StandardErrorPath = "${config.users.users.ldangelo.home}/Library/Logs/kanata.err.log";
      ProcessType = "Interactive";
      Nice = -20;
    };
  };

  # Jankyboarders: enable
  services.jankyborders.enable = true;
}
