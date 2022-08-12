{
  description = "Haskell Optimization Handbook flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };};

  outputs = { self, nixpkgs, flake-utils, flake-compat }:
    let press-theme-overlay = final: prev: {
          sphinx-press-theme = prev.python310Packages.buildPythonPackage rec {
            pname = "sphinx_press_theme";
            version = "0.8.0";

            src = prev.python3Packages.fetchPypi {
              inherit pname;
              inherit version;
              sha256 = "sha256-KITKqx3AHssR0VjU3W0xeeLdl81IUWx2nMJzYCcuYrM=";
            };
            propagatedBuildInputs = [ prev.sphinx ];
          };
        };
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let # pkgs = nixpkgs.legacyPackages.${system};
            pkgs = import nixpkgs
              { inherit system;
                overlays = [ press-theme-overlay ];
              };

            ourTexLive = pkgs.texlive.combine {
              inherit (pkgs.texlive)
                scheme-medium collection-xetex fncychap titlesec tabulary varwidth
                framed capt-of wrapfig needspace dejavu-otf helvetic upquote;
            };

            ## TODO use this
            fonts = pkgs.makeFontsConf { fontDirectories = [ pkgs.dejavu_fonts ]; };

            buildHoh = { target ? "html"
                       }:
                         pkgs.stdenv.mkDerivation {
                           pname   = "hoh";
                           version = "0.0.1";
                           src     = ./.;


                           propagatedBuildInputs = with pkgs; [
                             python310Packages.sphinx
                             python310Packages.sphinx-autobuild
                             python310Packages.sphinxcontrib-bibtex
                             python310Packages.sphinxcontrib-tikz
                             sphinx-press-theme
                             pandoc
                             rst2html5
                             ## until we have a reason for tex leave this commented out for CI
                             # ourTexLive
                           ];

                           buildPhase = ''
                           make ${target}
                           touch "_build/.nojekyll"
                           touch "_build/html/.nojekyll"
                           '';
                         };
        in

        rec {
          packages = {
            default = import ./hoh.nix { inherit pkgs; target = "html"; };
            epub    = import ./hoh.nix { inherit pkgs; target = "epub"; };
            pdf     = import ./hoh.nix { inherit pkgs; target = "pdf"; };
          };
          devShells = {
            default = packages.default;
          };
        }
      );
}
