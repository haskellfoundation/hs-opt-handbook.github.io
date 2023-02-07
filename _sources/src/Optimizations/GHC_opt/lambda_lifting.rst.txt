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
default method GHC uses for handling local functions and free variables.
Instead, GHC uses an alternative strategy called :term:`Closure Conversion`,
which creates more uniformity at the cost of extra heap allocation.

Automated lambda lifting in GHC occurs *late* in the compiler pipeline at STG,
right before code generation. GHC lambda lifts at STG instead of Core because
lambda lifting interferes with other optimizations.

Lambda lifting in GHC is also *Selective*. GHC uses a cost model that calculates
hypothetical heap allocations a function will induce. GHC lists heuristics for
when *not* to lambda lift in `Note [When to lift]
<https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Stg/Lift/Analysis.hs#L46>`_
, we repeat the basic ideas here. See :cite:t:`selectiveLambdaLifting`, and the
`lambda lifting wiki
<https://gitlab.haskell.org/ghc/ghc/-/wikis/late-lam-lift>`_ entry for more
details.

GHC does not lambda lift:

#. :term:`Top-level` bindings. By definition these
   cannot be lifted.
#. :term:`Thunk` and Data Constructors. Lifting either of these would destroy
   sharing.
#. :term:`Join Point` because there is no lifting possible in a join point.
   Similarly, abstracting over join points destroys the join point by turning it
   into an argument to a lifted function.
#. Any local :term:`known function`. This would turn a known function call into
   an :term:`unknown function` call, which is slower. The flag
   ``-fstg-lift-lams-known`` disables this restriction and enables lifting of
   known functions.
#. Any function whose lifted form would have a higher arity than the available
   number of registers for the function's calling convention. See flags
   ``-fstg-lift-(non)rec-args(-any)``
#. Any function whose lifted form will result in *closure grawth*. Closure
   growth occurs when formerly free variables, that are now additional
   arguments, did not previously occur in the closure, thereby increasing
   allocations. This is especially bad for :term:`multi-shot` lambdas which will
   allocate many times.


Observing the Effect of Lambda Lifting
--------------------------------------

You may directly observe the effect of late lambda lifting by comparing Core to
STG when late lambda lifting is enabled. You can also directly disable or enable
late lambda lifting with the flags ``-f-stg-lift-lams`` and
``-fno-stg-lift-lams``. In general, lambda lifting performs the following
syntactic changes:

#. It eliminates a let binding.
#. It creates a new :term:`Top-level` binding.
#. It replaces all occurrences of the lifted function in the let's body with a
   partial application. For example, all occurrences of ``f`` are replaced with
   ``$lf b`` in the let's body.
#. All non-top-level variables (i.e., free variables) in the let's body become
   occurrences of parameters.

When to Manually Apply Lambda Lifting
-------------------------------------

tomorrow: update glossary, start here


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
