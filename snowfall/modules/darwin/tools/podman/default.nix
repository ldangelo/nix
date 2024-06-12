{config, lib, namespace, pkgs, ...}:
let
  cfg = config.${namespace}.tools.podman;
  inherit (lib) mkIf;
  inherit (lib.${namespace}) mkBoolOpt;

  # Work around https://github.com/containers/podman/issues/17026
  # by downgrading to qemu-8.1.3.
  inherit (import (pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "4db6d0ab3a62ea7149386a40eb23d1bd4f508e6e";
    sha256 = "sha256-kyw7744auSe+BdkLwFGyGbOHqxdE3p2hO6cw7KRLflw=";
  }) { inherit (pkgs) system; }) qemu;

in 
{

  options.${namespace}.tools.podman = {
    enable = mkBoolOpt false "Whether or not to enable podman.";
    masEnable = mkBoolOpt false ;
  };

  config = mkIf cfg.enable {
      environment.systemPackages = [
          pkgs.podman
          qemu
          pkgs.xz
      ];

      environment.pathsToLink = [ "/share/qemu" ];

       # https://github.com/LnL7/nix-darwin/issues/432#issuecomment-1024951660
      environment.etc."containers/containers.conf.d/99-gvproxy-path.conf".text = ''
            [engine]
            helper_binaries_dir = ["${pkgs.gvproxy}/bin"]
          '';
    };
}
