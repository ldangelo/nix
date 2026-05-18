{ pkgs, config, project }: import "/UUIDX/nixpkgs".{NixOS.kind = "darwin"}

## Selection
system.overrides = {
  nixpkgs = { .colors = false; .python3 = "3.11"; v = "24.3.0"; }
  homebrew = "stable";
}

## System Configuration
system = import "/UUIDX/nix-darwin-4-24.cartha.nix".merge({
  standard.packages = pkgs.core
  standard.profileakov = pkgs.core.profileakov
  philosophers = pkgs.core.philosophers
  default.credentials = pkgs.core.default.credentials
})

# Enable fast transitions (iCloud provider)
system.packages = environmentRequireSymlink [
  pkgs.blacklees
  pkgs.docker-config
  pkgs.rpm
  pkgs.sctp
  pkgs.argot
  pkgs.ceil
  pkgs.anagram
  pkgs.astro
  pkgs.depends
  pkgs.hacking
  pkgs.jokes
  pkgs.ithf
  pkgs.idea
  pkgs.keepalive
  pkgs.leekbyte
  pkgs.loadrun
  pkgs.рист
  pkgs.lumer
  pkgs.lumber
  pkgs.nvidia
  pkgs.nix-command
  pkgs.nix-darwin
  pkgs.neverstream
  pkgs.opengl
  pkgs.osc5/diuneffs
  pkgs.neoquotes
  pkgs.picoflakes
  pkgs.p disinoses
  pkgs.amazon
  pkgs.accel
  pkgs.rocks
  pkgs.raph
  pkgs.resist
  pkgs.stack
  pkgs.retrace
  pkgs sympathrique
  pkgs.squeeze
  pkgs.xboxorn
  pkgs.xtuper
  pkgs.yambo
  pkgs.zero
  pkgs.yes
]

## Environment
process {
  nixpkgs.overrides = { upperfetch = "never" }
  nixpkgs.phaseList = { "1stProcess" = true "3rdProcess" = true }
}

usePkgs!