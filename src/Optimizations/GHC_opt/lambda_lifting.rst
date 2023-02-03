.. _Lambda Lifting Chapter:

   Local Variables
.. |glift| replace:: ``g_lifted``

`Lambda Lifting`
================

Lambda Lifting :cite:p:`lambdaLifting` is a classic rewriting technique that
that avoids excess closure allocations. It avoids closure allocation by moving
local functions to the global scope of the program, and by adding parameters to
the function definition to capture free variables. Thus, when a lifted function
is called no heap allocation is needed because the lifted function no longer
contains closures, rather it only references global names.

A Working Example
-----------------

Consider the following program [#]_:

.. code-block:: haskell

   f :: Int -> Int -> Int
   f a 0 = a
   f a n = f (g (n `mod` 2)) (n - 1)
     where
       g 0 = a
       g n = 1 + g (n - 1)

The function ``f`` defines one local function, ``g``, which appears as a free
variable in ``f``. Similarly, the variable ``a`` is a free variable in ``g``. A
lambda lifted ``g``, will convert all free variables in ``g`` to parameters.
Thus |glift| turns into:

.. code-block:: haskell

   g_lifted a 0 = a
   g_lifted a n = 1 + g (n - 1)

Now ``a`` is an input, which means that |glift| can be floated out of ``f``
to the top level producing the final program:

.. code-block:: haskell

   g :: Int -> Int -> Int
   g_lifted a 0 = a
   g_lifted a n = 1 + g (n - 1)

   f :: Int -> Int -> Int
   f a 0 = a
   f a n = f (g_lifted a (n `mod` 2)) (n - 1)

This new program will be much faster because ``f`` becomes essentially
non-allocating. Before the lambda lifting transformation ``f`` had to allocate a
closure for ``g`` in order to pass ``a`` to ``g``. After the lambda lifting on
``g`` this is no longer the case; |glift| is a top level function so ``f`` can
simply reference it; no closures needed!

.. note::

   The fundamental tradeoff is decreased heap allocation for an increase in
   function parameters at each call site. This means that lambda lifting is not
   always a performance win. See `When to Manually Apply Lambda Lifting`_ for
   guidance on recognizing when your program may benefit.


How Lambda Lifting Works in GHC
-------------------------------

GHC does have a lambda lifting pass in STG, however lambda lifting is not the
default method GHC uses for handling local functions. GHC uses an alternative
strategy called :term:`Closure Conversion`...


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


.. [#] This program and example comes from Sebastian Graf and Simon Peyton Jones
       :cite:p:`selectiveLambdaLifting`; thank you for your labor!:
