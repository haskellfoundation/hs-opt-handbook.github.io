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

     #######################################################################
            custom_nodeenv = pkgs.python3Packages.buildPythonPackage rec {
              pname = "nodeenv";
              version = "1.6.0";

              src = pkgs.python3Packages.fetchPypi {
                inherit pname;
                inherit version;
                sha256 = "sha256-PvE/+QKRuipKek/5qXm2P/3QCkZNvgSs8OpkcVF6TCs=";
              };

              propagatedBuildInputs = with pkgs.python3Packages; [
                coverage
                flake8
                mock
                pytest
                tox
              ];

              doCheck = false; # no tests
            };

     #######################################################################
            pep621 = pkgs.python3Packages.buildPythonPackage rec {
              pname = "pep621";
              version = "0.4.0";

              ## required because the project doesn't have setup.py but does have pyproject.toml
              format = "pyproject";

              src = pkgs.python3Packages.fetchPypi {
                inherit pname;
                inherit version;
                sha256 = "sha256-AkJxtCw85y/UxXeS2kKMSmwp+t7ljDWACW4qGoYTFDQ=";
              };

              propagatedBuildInputs = with pkgs.python3Packages; [
                packaging
              ];

              doCheck = false; # no tests

              pythonImportsCheck = [ "pep621" ];
            };

     #######################################################################
            sphinx-theme-builder = pkgs.python3Packages.buildPythonPackage rec {
              pname = "sphinx-theme-builder";
              version = "0.2.0a13";

              ## required because the project doesn't have setup.py but does have pyproject.toml
              format = "pyproject";

              src = pkgs.python3Packages.fetchPypi {
                inherit pname;
                inherit version;
                sha256 = "sha256-0L9EKanAUZEO7pSjKUL6lA21Rv0DOnzviYbI6mjyP8U=";
              };

              propagatedBuildInputs = with pkgs.python3Packages; [
                flit-core
                tomli
                packaging
                toolz
                pep621
                custom_nodeenv
                rich
              ];

              doCheck = false; # no tests

              # pythonImportsCheck = [ "sphinx-theme-builder[cli]" ];
            };

     #######################################################################
            sphinx-press-theme = pkgs.python3Packages.buildPythonPackage rec {
              pname = "sphinx_press_theme";
              version = "0.8.0";

              src = pkgs.python3Packages.fetchPypi {
                inherit pname;
                inherit version;
                sha256 = "sha256-KITKqx3AHssR0VjU3W0xeeLdl81IUWx2nMJzYCcuYrM=";
              };

              propagatedBuildInputs = with pkgs.python3Packages; [
                sphinx
              ];

              doCheck = false; # no tests

              # pythonImportsCheck = [ "sphinx-theme-builder[cli]" ];
            };


     #######################################################################
            sphinx-book-theme = pkgs.python3Packages.buildPythonPackage rec {
              pname = "sphinx-book-theme";
              version = "0.3.0";

              ## required because the project doesn't have setup.py but does have pyproject.toml
              format = "pyproject";

              src = pkgs.python3Packages.fetchPypi {
                pname = "sphinx_book_theme";
                inherit version;
                sha256 = "sha256-7PRqosNCVfgkg7RMErGpJBcvZCDD8x6bs05herf4jbI=";
              };

              buildInputs = with pkgs; [nodejs];

              propagatedBuildInputs = with pkgs.python3Packages; [
                sphinx
                sphinx-theme-builder
                # beautifulsoup4
                # python-slugify
                # unidecode
                # css-html-js-minify
                # lxml
              ];

              preBuildPhase = ''
              nodeenv --prebuilt
              nodejs
              '';

              doCheck = false; # no tests

              pythonImportsCheck = [ "sphinx_book_theme" ];
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


                           buildInputs = with pkgs; [
                             python3Packages.sphinx
                             python3Packages.sphinx-autobuild
                             # sphinx-book-theme
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
          defaultPackage = packages.build;

          packages = {
            build = buildHoh { target = "html"; };
            epub  = buildHoh { target = "epub"; };
            pdf   = buildHoh { target = "pdf"; };
          };
        }
      );
}
