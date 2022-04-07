{stdenv, sphinx, pandoc , target ? "html"
, texlive
}:
stdenv.mkDerivation {
  pname   = "hoh";
  version = "0.0.1";
  src     = ./.;


  buildInputs = [
    sphinx pandoc texlive.combined.scheme-full
  ];


  ## TODO: perhaps just use nix and not make?
  buildPhase = ''
  make ${target}
  '';
}
