{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let hoh = callPackage ./hoh.nix { };

in rec {
  inherit hoh;
  ## could override if this gets too hairy
  hoh-pdf   = callPackage ./hoh.nix { target = "latexpdf"; };
  hoh-epub  = callPackage ./hoh.nix { target = "epub";     };
  hoh-shell = mkShell { buildInputs = hoh.buildInputs;     };
}
