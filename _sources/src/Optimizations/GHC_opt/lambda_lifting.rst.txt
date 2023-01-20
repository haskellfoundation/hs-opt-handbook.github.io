.. _Lambda Lifting Chapter:

`Lambda Lifting`
================

Lambda Lifting is a classic optimization technique...


Testing Exec

.. exec::
   :context: true
   :process: haskell

   module Main where

   main :: IO ()
   main = do
     let x = fmap (+10) [1..10]
     print x


Great that worked now lets try ``ghci``


.. exec::
   :context: true
   :process: haskell
   :with: ghci

   :t "Hello"

and also we can load a package

.. exec::
   :context: true
   :process: haskell
   :with: ghci

   :m + Data.List
   :t span

and we can also run from cabal target!

.. exec:: code/lethargy/bench/Weigh/Main.hs
   :context: true
   :process: haskell
   :project_dir: code/lethargy/
   :with: cabal
   :args: bench lethargy:weigh --dry-run
