{ pkgs, ...}@args:
with pkgs;
stdenv.mkDerivation {
  pname = "lethargy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildInputs = [ ghc haskell-language-server cabal-install haskellPackages.eventlog2html haskellPackages.ghc-events ];
  executableHaskellDepends = with haskellPackages; [ base ];
  benchmarkHaskellDepends =  with haskellPackages;[ base containers deepseq gauge random ];
  mainProgram = "lethargy";
}
