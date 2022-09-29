{
  description = "A Flake for code examples";

  inputs = {
    flake-utils.url =  "github:numtide/flake-utils";
    flake-compat    = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };};

  outputs = { self, nixpkgs, flake-utils, flake-compat }:
    let compiler = "ghc8107";
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = import nixpkgs { inherit system; };
        in

        rec {
          packages = {
            default = import ./lethargy.nix { inherit pkgs;
                                              inherit compiler;
                                              doBenchmark = true;
                                            };
          };
          devShells = {
            default = packages.default;
          };
        }
      );
}
