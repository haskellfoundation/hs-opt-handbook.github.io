{ pkgs, compiler ? "default", doBenchmark ? false }:

with pkgs;

let
  ghc = haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
          base containers deepseq gauge lib random
        ]);

  f = { mkDerivation, base, containers, deepseq, gauge, lib, random
      , haskell-language-server
      }:
      pkgs.stdenv.mkDerivation {
        pname = "lethargy";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildInputs = [ ghc haskell-language-server ];
        executableHaskellDepends = [ base ];
        benchmarkHaskellDepends = [ base containers deepseq gauge random ];
        # license = lib.licenses.mit;
        mainProgram = "lethargy";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
