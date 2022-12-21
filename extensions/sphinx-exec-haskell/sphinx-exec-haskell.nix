{ pkgs }:

pkgs.buildPythonPackage rec {
  pname = "sphinx-exec-haskell";
  version = "0.10.0";

  src = ./src;

  doCheck = false;

  propogatedBuildInputs = [];

  meta = with pkgs.lib; {
    homepage    = "https://github.com/input-output-hk/hs-opt-handbook.github.io";
    description = "A sphinx extension to execute and test haskell code blocks";
    license     = licenses.bsd3;
    maintainers = with maintainers; [ doyougnu ];
  };
}
