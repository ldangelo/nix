{
  description = "Example flake with env vars";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs }: {
    devShells.default = nixpkgs.lib.mkShell {
      buildInputs = [ ];
      shellHook = ''
        export MY_VAR="my_value"
        export ANOTHER_VAR="another_value"
      '';
    };
  };
}

