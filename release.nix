{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let hoh = callPackage ./hoh.nix { };

in rec {
  inherit hoh;
  ## could override if this gets too hairy
  hoh-pdf   = callPackage ./hoh.nix { target = "pdf";  };
  hoh-html  = callPackage ./hoh.nix { target = "html"; };
  hoh-shell = mkShell { buildInputs = hoh.buildInputs; };
}
