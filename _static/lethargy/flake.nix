{
  description = "A Flake for code examples";

  inputs = {
    flake-utils.url =  "github:numtide/flake-utils";
    flake-compat    = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };};

  outputs = { self
            , nixpkgs
            , flake-utils
            , flake-compat
            , ...
            }@attrs :
    let
      version  = "924";
      compiler = "ghc${version}";

        overlay-ghc = final: prev: {
          ghc = prev.haskell.packages.${compiler}.ghcWithHoogle (hp: with hp;
            [ base containers deepseq gauge random unordered-containers stm
              arrows bytestring array directory process
            ]);

          haskell-language-server = prev.haskell-language-server.override
            { supportedGhcVersions = [ version ];};
      };
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = import nixpkgs { inherit system ;
                                    overlays = [ overlay-ghc ];
                                  };
            lethargy = pkgs.callPackage ./lethargy.nix { inherit pkgs; };
        in

        rec {
          packages = {
            default = lethargy;
          };
          devShells = {
            default = packages.default;
          };
        }
      );
}
