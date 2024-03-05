.. _Reading Asm:

Reading Asm
===========

..
   we must use --no-sandbox with nix or else this will fail
.. exec::
   :context: true
   :process: haskell
   :project_dir: code/lethargy
   :with: cabal
   :args: run lethargy-asm
   :output: comp
   :intertext:  Core output (the 0 at the end is the program's result):

