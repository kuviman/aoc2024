{
  description = "A devShell example";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    kast.url = "github:kast-lang/kast";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, kast, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
      with pkgs;
      {
        devShells.default = mkShell {
          packages = [
            kast.packages.${system}.default
          ];
        };
      }
    );
}
