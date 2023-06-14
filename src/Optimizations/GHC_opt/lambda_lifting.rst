.. _Lambda Lifting Chapter:

..
   Local Variables
.. |glift| replace:: ``g_lifted``

`Lambda Lifting`
================

Lambda Lifting :cite:p:`lambdaLifting` is a classic rewriting technique that
avoids excess closure allocations and removes free variables from a function. It
avoids closure allocation by moving local functions out of an enclosing function
to the :term:`top-level`. It then removes free variables by adding parameters to
the lifted function that captures the free variables. This chapter describes the
lambda lifting transformation, how GHC implements the transformation, and
provides guidance for when to implement the transformation manually.

A Working Example
-----------------

Consider the following program [#f1]_:

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
   g_lifted a n = 1 + g_lifted a (n - 1)

Now ``a`` is an input, which means that |glift| can be floated out of ``f``
to the top level producing the final program:

.. code-block:: haskell

   g_lifted :: Int -> Int -> Int
   g_lifted a 0 = a
   g_lifted a n = 1 + g_lifted a (n - 1)

   f :: Int -> Int -> Int
   f a 0 = a
   f a n = f (g_lifted a (n `mod` 2)) (n - 1)

Before the lambda lifting transformation ``f`` had to allocate a closure for
``g`` in order to allow ``g`` to reference ``a``. After the lambda lifting on
``g`` this is no longer the case; |glift| is a top level function so ``f`` can
simply reference it; no closures needed!

This new program *could* be much faster than the original, it depends on the
usage patterns the programs will experience. To understand the distribution of
patterns inspect the function's behavior with respect to its inputs. The
original program allocates one expensive closure for ``g`` per call of ``f``. So
When ``n`` is large, there will be few calls of ``f`` relative to ``g``, in fact
for each call of ``f`` we should expect exactly ``n `mod` 2`` calls of ``g``. In
this scenario, the original program is faster because it allocates some closures
in the outer loop (``f``, the outer loop, allocates a closure for ``g``, the
inner loop, which includes a reference to ``a``) and in turn saves allocations
in the inner loop (``g``) because ``a`` can simply be referenced in ``g``. Since
the inner loop is called much more than the outer loop this pattern saves
allocations.

In contrast, the lifted version must allocate an additional argument for ``a``
*for each* call of ``g_lifted``. So when ``n`` is large and we have many more
calls to ``g_lifted`` relative to ``f`` the extra argument required to pass
``a`` adds up to more allocations than the original version would make.

However the situation reverses when there are *many* calls to ``f a n`` with a
small ``n``. In this scenario, the closure allocation that the original makes in
the outer loop do not pay off, because the inner loop is relatively short lived
since ``n`` is small. For the same reason, the lambda lifted version is now
fruitful: because ``n`` is small the extra parameter that |glift| must allocate
stays cheap. Thus the lifted version is faster by avoiding the closure
allocation in the now frequently called outer loop.

Now ``f`` is an obviously contrived example, so one may ask how frequently the
many-calls with low ``n`` scenario will occur in practice. The simplest example
is very familiar:

.. code-block:: haskell

   -- | map with no lambda lifting
   map f = go
     where
       go []     = []
       go (x:xs) = f x : go xs

vs. the lifted version:

.. code-block:: haskell

   -- | map lambda lifted
   map f []     = []
   map f (x:xs) = f x : map f xs

The first form is beneficial when there are a few calls on long lists via the
same reasoning as above; only now we have the list determines the number of
calls instead of ``n`` and ``f`` is free rather than ``a`` . Similarly, the
second form is beneficial when there many calls of ``map`` on short lists.

.. note::

   The fundamental tradeoff is decreased heap allocation for an increase in
   function parameters at each call site. This means that whether lambda lifting
   is a performance win or not depends on the usage pattern of the function as
   we have demonstrated. See :ref:`When to Manually Apply Lambda Lifting <when>`
   for guidance on recognizing when your program may benefit. In general,
   closure allocation is more expensive than pushing an extra parameter onto the
   stack.


How Lambda Lifting Works in GHC
-------------------------------

GHC does have a lambda lifting pass in STG, however lambda lifting is not the
default method GHC uses for handling local functions and free variables.
Instead, GHC uses an alternative strategy called :term:`Closure Conversion`,
which creates more uniformity at the cost of extra heap allocation.

Automated lambda lifting in GHC is called *late lambda lifting* because it
occurs in the compiler pipeline in STG, right before code generation. GHC lambda
lifts at STG instead of Core because lambda lifting interferes with other
optimizations.

Lambda lifting in GHC is also *Selective*. GHC uses a cost model that calculates
hypothetical heap allocations a function will induce. GHC lists heuristics for
when *not* to lambda lift in `Note [When to lift]
<https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Stg/Lift/Analysis.hs#L46>`_
, we repeat the basic ideas here. See :cite:t:`selectiveLambdaLifting`, and the
:ghcWiki:`lambda lifting wiki <late-lam-lift>` entry for more details.

GHC does not lambda lift:

#.  A :term:`top-level` binding. By definition these cannot be lifted.
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
   allocations. This is especially bad for any :term:`multi-shot lambda`, which
   will allocate many times.


Observing the Effect of Lambda Lifting
--------------------------------------

You may directly observe the effect of late lambda lifting by comparing Core to
STG when late lambda lifting is enabled. You can also disable or enable late
lambda lifting with the flags ``-f-stg-lift-lams`` and ``-fno-stg-lift-lams``.
In general, lambda lifting performs the following syntactic changes:

#. It eliminates a let binding.
#. It creates a new :term:`top-level` binding.
#. It replaces all occurrences of the lifted function in the let's body with a
   partial application. For example, all occurrences of ``f`` are replaced with
   ``$lf b`` in the let's body.
#. All non-top-level variables (i.e., free variables) in the let's body become
   occurrences of parameters.

.. _when:

When to Manually Lambda Lift
----------------------------

GHC does a good job finding beneficial instances of lambda lifting. However, you
might want to manually lambda lift to save compile time, or to increase the
performance of your program without relying on GHC's optimizer.

When deciding when to manually lambda lift, consider the following:

1. What is the expected usage pattern of the functions.
2. How many more parameters would be passed to these functions.

Let's take these in order: (1) lambda lifting trades heap (the let bindings that
it removes), for stack (the increased function parameters). Thus whether or not
it is a performance win depends on the usage patterns of the enclosing function
and to-be lifted function. As demonstrated in the motivating example,
performance can degrade when extra parameter in combination with the usage
pattern of the function results in more total allocation during the lifetime of
the program. Performance may also degrade if the existing closures grow as a
result of the lambda lift. Both kinds of extra allocation slow the program down
and increases pressure on the garbage collector. So it is important to learn to
read the program from the perspective of memory. Consider this example from
:cite:t:`selectiveLambdaLifting`:

.. code-block:: haskell

   -- unlifted.

   -- f's increases heap because it must have a closure that includes the 'x'
   -- and 'y' free variables

   -- 'g' increases heap because of the let and must have 'f' and 'x' in its
   -- closure (not assuming other optimizations such as constant propagation)

   -- 'h' increases heap because 'f' is free in 'h'

   let f a b = a + x + b + y
       g d   = let h e = f e e
               in h x
   in g 1 + g 2 + g 3

Let's say we lift ``f``, now we have:


.. code-block:: haskell

   -- lifted f

   f_lifted x y a b = a + x + b + y

   let g d   = let h e = f_lifted x y e e
               in h x
   in g 1 + g 2 + g 3

``f_lifted`` is now a top level function, thus any closure that contained ``f``
before the lift will save one slot of memory. With ``f_lifted`` we additionally
save two slots of memory because ``x`` and ``y`` are now parameters. Thus
``f_lifted`` does not need to allocate a closure with :term:`Closure
Conversion`. ``g``'s allocations do not change since ``f_lifted`` can be
directly referenced just as before and because ``x`` is still free in ``g``. So
``g``'s closure will contain ``x`` and ``f_lifted`` will be inlined, same as
``f`` in the unlifted version. ``h``'s allocations grow by one slot since ``y``
*is now also* free in ``h``, just as ``x`` was. So it would seem that in total
lambda lifting ``f`` saves one slot of memory because two slots were lost in
``f`` and one was gained in ``h``. However, ``g`` is a :term:`multi-shot
lambda`, which means ``h`` will be allocated *for each* call of ``g``, whereas
``f`` and ``g`` are only allocated once. Therefore, the lift is a net loss.

This example illustrates how tricky good lifts can be. To estimate allocations
counting the ``let`` expressions, the number of free variables,
and the number of times the outer function and inner functions are expected to
be called.

.. note::

   Recall, due to closure conversion GHC allocates one slot of memory for each
   free variable. Local functions are allocated *once per call* of the enclosing
   function. Top level functions are always only allocated once.

(2) The next determining factor is counting the number of new parameters that is
passed to the lifted function. Should this number become greater than the number
of available argument registers on the target platform then you'll incur slow
downs in the STG machine. These slowdowns result from more work the STG machine
will need to do; it will need to generate code that pops arguments from the
stack instead of just applying the function to arguments that are already loaded
into registers. In a hot loop this extra manipulation can have a large impact.

In general the heuristic is: if there are few calls to the outer loop and many
calls to the inner loop, then do not lambda lift. However, if there are many
calls to the outer loop and few calls made in the inner loop, then lambda
lifting will be beneficial.

Summary
-------

#. Lambda lifting is a classic optimization technique for compiling local
   functions and removing free variables.
#. Lambda lifting trades heap for stack. To determine if a manual lambda lift
   would be beneficial determine the use pattern of the enclosing and local
   functions, determine if closures would grow in the lifted version, and ensure
   that the extra parameters in the lifted version would not exceed the number
   of argument registers on the platform the program targets.
#. GHC automatically performs lambda lifting, but does so only selectively. This
   transformation is late in the compilation pipeline at STG and right before
   code generation. GHC's lambda lifting transformation can be toggled via the
   ``-f-stg-lift-lams`` and ``-fno-stg-lift-lams`` flags.
#. To tell if your program has undergone lifting you can compare the Core with
   the STG. Or, you may compare STG with and without lifting explicitly enabled.

.. [#f1] This program and example comes from :cite:t:`selectiveLambdaLifting`;
         thank you for your labor!:
