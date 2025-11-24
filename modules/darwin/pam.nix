{ config, pkgs, ... }:

{
  # Enable Touch ID and Apple Watch authentication for sudo
security.pam.services.sudo_local.touchIdAuth = true;
}
