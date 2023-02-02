.. _Lambda Lifting Chapter:

`Lambda Lifting`
================

Lambda Lifting :cite:p:`lambdaLifting` is a classic rewriting technique that
rewrites functions to avoid excess allocations. It optimizes function
definitions by moving local functions to the global scope of the program;
thereby closure allocations, and by adding parameters to the function definition
to capture free variables.

A Working Example
-----------------

Consider the following program [#]_:

.. exec::
   :context: true
   :process: haskell

   module Main where

   f :: Int -> Int -> Int
   f a 0 = a
   f a n = f (g (n `mod` 2)) (n - 1)
     where
       g 0 = a
       g n = 1 + g (n - 1)

   main :: IO ()
   main = print $ f 10 100


How Lambda Lifting Works in GHC
-------------------------------


Observing the Effect of Lambda Lifting
--------------------------------------

When to Manually Apply Lambda Lifting
-------------------------------------




Testing Exec

.. exec::
   :context: false
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

and we can also run from cabal target!!

.. exec:: code/lethargy/bench/TooManyClosures.hs
   :context: true
   :process: haskell
   :project_dir: code/lethargy/
   :with: cabal
   :args: bench lethargy:tooManyClosures


.. [#] This program comes from Sebastian Graf and Simon Peyton Jones
       :cite:p:`selectiveLambdaLifting`; thank you for your labor!:
