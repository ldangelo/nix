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
      UserName = "root";
      GroupName = "wheel";
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
      KeepAlive = true;
      StandardOutPath = "/var/log/karabiner-vhidmanager.out.log";
      StandardErrorPath = "/var/log/karabiner-vhidmanager.err.log";
      UserName = "root";
      GroupName = "wheel";
     };
  };

  # Kanata - Advanced keyboard remapper (system-level daemon)
  # https://github.com/jtroo/kanata
  # Must be a daemon (not agent) to run as root - agents ignore UserName
  launchd.daemons.kanata = {
    serviceConfig = {
      Label = "com.ldangelo.kanata";
      ProgramArguments = [
        "/opt/homebrew/bin/kanata"
        "--cfg"
        "/Users/ldangelo/.config/kanata/home-row.kbd"
      ];
      KeepAlive = true;
      RunAtLoad = true;
      StandardOutPath = "/var/log/kanata.out.log";
      StandardErrorPath = "/var/log/kanata.err.log";
      ProcessType = "Interactive";
      Nice = -20;
      UserName = "root";
      GroupName = "wheel";
    };
  };

  # pizauth: authentication proxy
  launchd.daemons.pizauth = {
    serviceConfig = {
      Label = "com.ldangelo.pizauth";
      ProgramArguments = [
        "pizauth"
        "server"
      ];
      KeepAlive = true;
      RunAtLoad = true;
      StandardOutPath = "/var/log/pizauth.out.log";
      StandardErrorPath = "/var/log/pizauth.err.log";
      ProcessType = "Interactive";
      Nice = -20;
      UserName = "root";
      GroupName = "wheel";
    };
  };

  # Jankyboarders: enable
  services.jankyborders = {
      enable = true;
      width = 6.0;
      hidpi = false;
      active_color =  "0xffe2e2e3";
      inactive_color = "0xff414550";
    };


}
