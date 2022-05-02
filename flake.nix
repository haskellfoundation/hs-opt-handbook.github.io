{
  description = "Haskell Optimization Handbook flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };};

  outputs = { self, nixpkgs, flake-utils, flake-compat }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system};

            buildHoh = { target ? "html"
                       }:
                         pkgs.stdenv.mkDerivation {
                           pname   = "hoh";
                           version = "0.0.1";
                           src     = ./.;


                           buildInputs = with pkgs; [
                             sphinx pandoc texlive.combined.scheme-full
                           ];

                           buildPhase = ''
                           make ${target}
                           '';
                         };
        in
        rec {

          defaultPackage = packages.build;

          packages = {
            build = buildHoh { target = "html"; };
          };
        }
      );
}
