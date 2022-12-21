{ pkgs }:

with pkgs;

python3.pkgs.buildPythonPackage rec {
  pname   = "sphinx-exec-haskell";
  version = "0.10.0";
  format  = "pyproject";

  src = ./.;

  doCheck = false;

  propogatedBuildInputs = with python3.pkgs; [ pip ];

  meta = with lib; {
    homepage    = "https://github.com/input-output-hk/hs-opt-handbook.github.io";
    description = "A sphinx extension to execute and test haskell code blocks";
    license     = licenses.bsd3;
    maintainers = with maintainers; [ doyougnu ];
  };
}
