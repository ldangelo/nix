{ config, pkgs, ... }:

{
  # Enable Touch ID and Apple Watch authentication for sudo
  security.pam.enableSudoTouchIdAuth = true;
}
