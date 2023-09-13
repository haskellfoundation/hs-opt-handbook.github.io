{ pkgs
, target ? "html"
}:

let
   pythonInputs = with pkgs.python310Packages; [
     sphinx
     sphinxcontrib-bibtex
     sphinxcontrib-tikz
     # marked as broken in nixpkgs unfortunately
     # sphinx-book-theme
     ## until we have a reason for tex leave this commented out for CI
     # ourTexLive
   ];
   nonPythonInputs = with pkgs; [ sphinx-press-theme # this comes from the overlay
                                  sphinx-copybutton  # this comes from the overlay
                                  pandoc
                                  # change once extension fixes are upstreamed
                                  # sphinx-exec-directive
                                  rst2html5
                                  sphinx-autobuild
                                  sphinx-exec-haskell
                                  ghc
                                  cabal-install
                                  git
                                ];
in
pkgs.stdenv.mkDerivation {
   pname   = "hoh";
   version = "0.0.1";
   src     = ./.;
   propagatedBuildInputs = pythonInputs ++ nonPythonInputs;

   # set SOURCE_DATE_EPOCH using git, fixes #58
   shellHook = ''
   SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct)
   '';

   buildPhase = ''
   runHook preBuild
   make ${target} SPHINXOPTS="-W"
   touch "_build/.nojekyll"
   touch "_build/html/.nojekyll"
   '';

   installPhase = ''
   mkdir -p $out/
   cp -r _build/${target}/ $out/
   '';
}
