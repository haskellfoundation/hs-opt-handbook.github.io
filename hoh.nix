{ pkgs
, target ? "html"
}:

let
   pythonInputs = with pkgs.python311Packages; [
     sphinx
     sphinxcontrib-bibtex
     sphinxcontrib-tikz
     sphinx-autobuild
     pip
     # marked as broken in nixpkgs unfortunately
     # sphinx-book-theme
     ## until we have a reason for tex leave this commented out for CI
   ];
   nonPythonInputs = with pkgs; [ sphinx-press-theme # this comes from the overlay
                                  sphinx-copybutton  # this comes from the overlay
                                  # pandoc
                                  # change once extension fixes are upstreamed
                                  sphinx-exec-directive
                                  rst2html5
                                  ghc
                                  cabal-install
                                  git
                                  tex-env
                                ];
in
pkgs.stdenv.mkDerivation {
   pname   = "hoh";
   version = "0.0.1";
   src     = ./.;
   phases = [ "unpackPhase" "preBuild" "buildPhase" "installPhase"];
   buildInputs = pythonInputs ++ nonPythonInputs;

   preBuild = ''
   unset SOURCE_DATE_EPOCH
   export CABAL_DIR=$(mktemp -d)
   cabal user-config update
   '';

   buildPhase = ''
   runHook preBuild
   export PATH="${pkgs.lib.makeBinPath (pythonInputs ++ nonPythonInputs)}:$PATH";
   SOURCE_DATE_EPOCH="$(${pkgs.coreutils}/bin/date '+%s')"
   make clean
   make ${target} SPHINXOPTS="-W"
   touch "_build/.nojekyll"
   touch "_build/html/.nojekyll"
   '';

   installPhase = ''
   mkdir -p $out/
   cp -r _build/${target}/ $out/
   '';
}
