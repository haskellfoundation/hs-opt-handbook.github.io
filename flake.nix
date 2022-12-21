{
  description = "Haskell Optimization Handbook flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    sphinx-exec-haskell.url = "path:./extensions/sphinx-exec-haskell";
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat, sphinx-exec-haskell }:
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

        copy-button-overlay = final: prev: {
          sphinx-copybutton = prev.python310Packages.buildPythonPackage rec {
            pname = "sphinx-copybutton";
            version = "0.5.0";

            src = prev.python3Packages.fetchPypi {
              inherit pname;
              inherit version;
              sha256 = "sha256-oMBZ2q3QPCe6dQ2lNKkqY+ejanc23PaE8m7jRhmXh/Y=";
            };
            propagatedBuildInputs = [ prev.sphinx ];
          };
        };

        sphinx-exec-haskell-overlay = sys: final: prev: {
          sphinx-exec-haskell = sphinx-exec-haskell.packages.${sys}.default;
        };

    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = import nixpkgs
              { inherit system;
                overlays = [ press-theme-overlay
                             copy-button-overlay
                             (sphinx-exec-haskell-overlay system)
                           ];
              };

            ourTexLive = pkgs.texlive.combine {
              inherit (pkgs.texlive)
                scheme-medium collection-xetex fncychap titlesec tabulary varwidth
                framed capt-of wrapfig needspace dejavu-otf helvetic upquote;
            };

            ## TODO use this
            fonts = pkgs.makeFontsConf { fontDirectories = [ pkgs.dejavu_fonts ]; };

        in

        rec {
          packages = {
            default = import ./nix/hoh.nix { inherit pkgs;
                                             target = "html";
                                           };

            epub    = import ./nix/hoh.nix { inherit pkgs;
                                             target = "epub";
                                           };

            pdf     = import ./nix/hoh.nix { inherit pkgs;
                                             target = "pdf";
                                           };
          };
          devShells = {
            default = packages.default;
          };
        }
      );
}
