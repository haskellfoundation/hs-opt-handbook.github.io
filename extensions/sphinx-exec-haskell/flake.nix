{
  description = "A flake for sphinx-exec-haskell";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };};

  outputs = { self, nixpkgs, flake-utils, flake-compat }:

    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = import nixpkgs { inherit system; };

        in

        rec {
          packages = {
            default = pkgs.poetry2nix.mkPoetryApplication {projectDir = self;};
            # default = import ./sphinx-exec-haskell.nix { inherit pkgs; };
          };
          devShells = {
            default = packages.default;
          };
        }
      );
}
