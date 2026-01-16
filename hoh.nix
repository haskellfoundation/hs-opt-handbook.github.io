{ pkgs
, target ? "html"
}:

let
   pythonEnv = pkgs.python313.withPackages (ps: with ps;
   [ sphinx
     sphinxcontrib-tikz
     sphinx-autobuild
     sphinxawesome-theme
     sphinx-copybutton  # this comes from the overlay
     sphinxcontrib-bibtex # from overlay
     # sphinx-exec-directive
     pip
   ]);

   nonPythonInputs = with pkgs; [ rst2html5
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
   buildInputs = [pythonEnv] ++ nonPythonInputs;

   preBuild = ''
   unset SOURCE_DATE_EPOCH
   export CABAL_DIR=$(mktemp -d)
   cabal user-config update
   '';

   buildPhase = ''
   runHook preBuild
   export PATH="${pkgs.lib.makeBinPath (nonPythonInputs)}:$PATH";
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
