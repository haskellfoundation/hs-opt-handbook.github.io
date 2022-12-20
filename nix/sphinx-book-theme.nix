{ lib
, buildPythonPackage
, fetchPypi
, sphinx
, beautifulsoup4
, python-slugify
, unidecode
, css-html-js-minify
, lxml
}:

buildPythonPackage rec {
  pname = "sphinx-book-theme";
  version = "0.0.1";

  src = fetchPypi {
    pname = "sphinx_book_theme";
    inherit version;
    sha256 = "08q8sfghnwxi13rk016j65b8l2y57z6i75kc0gwbqq5xsnav033h";
  };

  propagatedBuildInputs = [
    sphinx
    beautifulsoup4
    python-slugify
    unidecode
    css-html-js-minify
    lxml
  ];

  doCheck = false; # no tests

  pythonImportsCheck = [ "sphinx_book_theme" ];

  meta = with lib; {
    description = "A material-based, responsive theme inspired by mkdocs-material";
    homepage = "https://github.com/executablebooks/sphinx-book-theme";
    license = licenses.bsd3;
    maintainers = with maintainers; [ doyougnu ];
  };
}
