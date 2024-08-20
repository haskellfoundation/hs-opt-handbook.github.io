#!/usr/bin/env bash
#

# after a lot of hassle this simple solution is just better then sphinx-autobuild
# this is intended to be used from the root directory of the book

find . -name '*.rst'  | entr -sc 'nix build -L --no-sandbox && $(firefox --new-tab $(pwd)/result/html/index.html)'
