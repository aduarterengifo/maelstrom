{
  description = "maelstrom flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        commonInputs = [
          pkgs.jdk
          pkgs.graphviz
          pkgs.gnuplot
          pkgs.ruby
        ];
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = commonInputs;
        };
        packages.default = pkgs.buildEnv {
          name = "jdk-graphviz-env";
          paths = commonInputs;
        };
      }
    );
}
