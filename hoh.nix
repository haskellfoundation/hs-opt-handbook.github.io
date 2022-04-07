{stdenv, sphinx, pandoc , target ? "epub"
}:
stdenv.mkDerivation {
  pname   = "hoh";
  version = "0.0.1";
  src     = ./.;


  buildInputs = [
    sphinx pandoc
  ];


  ## TODO: perhaps just use nix and not make?
  buildPhase = ''
  make ${target}
  '';
}
