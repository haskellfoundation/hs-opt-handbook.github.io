{ pkgs
, target ? "html"
, sphinx-press-theme
}:

pkgs.stdenv.mkDerivation {
   pname   = "hoh";
   version = "0.0.1";
   src     = ./.;

   buildInputs = with pkgs; [
     python3Packages.sphinx
     python3Packages.sphinx-autobuild
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
}
