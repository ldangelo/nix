{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
#    pkgs.nix-tree
    cyrus_sasl
    cyrus-sasl-xoauth2
    isync-oauth2
    notmuch
    afew
#    offlineimap
    imapfilter
#    alot
    mu
   # pkgs.isync
  ];

  imports = [
    ./email
#    ./alacritty
  ];
}
