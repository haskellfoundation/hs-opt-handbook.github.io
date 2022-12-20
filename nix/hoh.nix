{ pkgs
, target ? "html"
}:

let
   pythonInputs = with pkgs.python310Packages; [
     sphinx
     sphinx-autobuild
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
                                  rst2html5
                                ];
in
pkgs.stdenv.mkDerivation {
   pname   = "hoh";
   version = "0.0.1";
   src     = ./.;
   buildInputs = pythonInputs ++ nonPythonInputs;
   # non python packages

   buildPhase = ''
   make ${target}
   touch "_build/.nojekyll"
   touch "_build/html/.nojekyll"
   '';
}
