{
  description = "Haskell Optimization Handbook flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat }:
    let press-theme-overlay = final: prev: {
          sphinx-press-theme = prev.python311Packages.buildPythonPackage rec {
            pname = "sphinx_press_theme";
            version = "0.8.0";

            src = prev.python311Packages.fetchPypi {
              inherit pname;
              inherit version;
              sha256 = "sha256-KITKqx3AHssR0VjU3W0xeeLdl81IUWx2nMJzYCcuYrM=";
            };
            propagatedBuildInputs = [ prev.sphinx ];
          };
        };

        copy-button-overlay = final: prev: {
          sphinx-copybutton = prev.python311Packages.buildPythonPackage rec {
            pname = "sphinx-copybutton";
            version = "0.5.2";

            src = prev.python311Packages.fetchPypi {
              inherit pname;
              inherit version;
              sha256 = "sha256-TPF8gvuWRtG8nKkqwoCBOjtgXYxCEiX9mRMVQQPuH70=";
            };
            propagatedBuildInputs = [ prev.sphinx ];
          };
        };

        sphinx-exec-directive-overlay = final: prev: {
          sphinx-exec-directive = prev.python311Packages.buildPythonPackage rec {
            pname   = "sphinx-exec-directive";
            version = "0.6";

            src = prev.python311Packages.fetchPypi {
              inherit pname;
              inherit version;
              sha256 = "sha256-lMo4QILqt6pEiIatN/LNxhiUGX3ziSWV+bfRahzmZWU=";
            };
            propagatedBuildInputs = [ prev.sphinx prev.python311Packages.matplotlib ];
          };
        };

        tex-overlay = final: prev: {
            tex-env = prev.texlive.combine {
              inherit (prev.texlive)
                scheme-basic collection-xetex fncychap titlesec tabulary varwidth
                framed capt-of wrapfig needspace dejavu-otf helvetic upquote
                memorygraphs;
            };
        };

    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = import nixpkgs
              { inherit system;
                overlays = [ press-theme-overlay
                             copy-button-overlay
                             sphinx-exec-directive-overlay
                             tex-overlay
                           ];
              } ;
            ## TODO use this
            fonts = pkgs.makeFontsConf { fontDirectories = [ pkgs.dejavu_fonts ]; };

        in

        rec {
          packages = {
            default = import ./hoh.nix { inherit pkgs;
                                         target = "html";
                                       };

            epub    = import ./hoh.nix { inherit pkgs;
                                         target = "epub";
                                       };

            pdf     = import ./hoh.nix { inherit pkgs;
                                         target = "pdf";
                                       };
          };
          devShells = {
            default = packages.default;
          };
        }
      );
}
