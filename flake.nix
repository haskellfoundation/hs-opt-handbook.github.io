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
    let pythonEnv-overlay = final: prev: {
      python313Packages = prev.python313Packages.overrideScope (pyFinal: pyPrev: {

          # sphinx-press-theme = pyPrev.buildPythonPackage rec {
            # pname = "sphinx-press-theme";
            # pyproject = true;
            # build-system = [ pyPrev.setuptools ];
            # version = "0.9.1";

            # src = pyPrev.fetchPypi {
              # inherit pname;
              # inherit version;
              # sha256 = "sha256-KITKqx3AHssR0VjU3W0xeeLdl81IUWx2nMJzYCcuYrM=";
            # };
            # propagatedBuildInputs = [ pyPrev.sphinx ];
          # };


          sphinx-copybutton = pyPrev.buildPythonPackage rec {
            pname = "sphinx-copybutton";
            pyproject = true;
            build-system = [ pyPrev.setuptools ];
            version = "0.5.2";

            src = pyPrev.fetchPypi {
              inherit pname;
              inherit version;
              sha256 = "sha256-TPF8gvuWRtG8nKkqwoCBOjtgXYxCEiX9mRMVQQPuH70=";
            };
            propagatedBuildInputs = [ pyPrev.sphinx ];
          };

          sphinx-exec-directive = pyPrev.buildPythonPackage rec {
            pname   = "sphinx-exec-directive";
            pyproject = true;
            build-system = [ pyPrev.setuptools ];

            version = "0.6";

            src = pyPrev.fetchPypi {
              inherit pname;
              inherit version;
              sha256 = "sha256-lMo4QILqt6pEiIatN/LNxhiUGX3ziSWV+bfRahzmZWU=";
            };
            propagatedBuildInputs = [ pyPrev.sphinx
                                      pyPrev.matplotlib
                                    ];
          };

          sphinxcontrib-bibtex = pyPrev.buildPythonPackage rec {
            pname   = "sphinxcontrib_bibtex"; # yes they are inconsistent with the name
            pyproject = true;
            build-system = [ pyPrev.setuptools ];

            version = "2.6.5";

            src = pyPrev.fetchPypi {
              inherit pname;
              inherit version;
              sha256 = "sha256-mzIk3W/s6SaOvYyQXcCoP/L2xUFIqSNf5w6dHp/xScA=";
            };
            propagatedBuildInputs = [ pyPrev.sphinx
                                      pyPrev.pybtex
                                      pyPrev.pybtex-docutils
                                    ];
          };
      });
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
                overlays = [ pythonEnv-overlay
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
