.. Weigh

`Weigh`
=======

`Weigh <https://hackage.haskell.org/package/weigh>`_ is a tiny Haskell package
to measure allocations of data constructors and functions. It provides a similar
interface to :ref:`Criterion, Gauge, and Tasty-Bench` and is useful to confirm
that a data structure or function has the memory performance you anticipate it
to have at runtime.

Requirements
------------

1. The program must not be compiled with ``-threaded``.
2. The program must not be compiled with profiling enabled.

Weigh works by tracking the allocation and garbage collection behavior of the
runtime system, but it does so by taking snapshots before and after forcing
whatever value you are passing to it. Thus, it will report incorrect
measurements if another thread changes the heap unexpectedly. Similarly, it will
report larger results if the values are artificially inflated for profiling.


What Information Do I Receive From Weigh?
--------------------------------------------

Weigh reports a table similar to ``criterion`` or ``gauge`` except displays
allocations in bytes and garbage collections. For example:

.. code-block:: bash

  Case       Allocated  GCs
  ()                 0    0
  1                  0    0
  True               0    0
  [0..3]           192    0

Notice that ``()``, ``1``, ``True`` do not allocate any memory. This is because
``weigh`` measures *heap* allocations. Thus, anything that GHC concludes can be
floated out of ``main`` and treated like a constant will return 0 allocations.
The list allocates due to the use of List Ranges. Contrast that with a static
literal version:

.. code-block:: bash

   Base             Allocated  GCs
   ()                       0    0
   1                        0    0
   True                     0    0
   [0..3]                 192    0
   [0,1,2,3]                0    0

which does no allocation as expected.


When should I use Weigh
-----------------------

Weigh is useful in the following scenarios:

- Inspecting the memory footprint of a value, such as a data type. This could be
  useful to determine whether your data type will fit in a CPU cache line or not.
- Inspecting the memory footprint of a function. This information could
  inform the decision to manually apply certain GHC optimizations that might not
  be firing. Such as Lambda Lifting or the SAT transformation.
- Inspecting the memory footprint of a data structure under a certain load. This
  is useful information to tune the data structure specifically for that load.
  For example, one might make tradeoffs that create slower writes in exchange
  for faster reads if the load requires exponentially more reads than writes.
- Tracking the memory requirements and garbage collection pressure for phases or
  sub-systems of your program, or even defining unit tests based on the output
  of weigh.

How should I use Weigh
----------------------

Weigh's interface is similar to criterion's and gauge's. Our recommendation is
to define a specific benchmark target to keep your project tidy and modular. For
example, for this chapter our toy programs are weighed through this cabal target:

.. code-block:: yaml

   benchmark weigh
     type            : exitcode-stdio-1.0
     default-language: Haskell2010
     ghc-options     : -O2 -fforce-recomp
     main-is         : Main.hs
     hs-source-dirs  : bench/Weigh
     build-depends: base >= 4.15
                  , containers
                  , deepseq
                  , weigh
                  , random

Then ``weigh`` exports an API to run your code with. Here is a list of common
functions to use:

- ``mainWith :: Weigh a -> IO ()``: Similar to ``defaultMain`` in criterion. This function is the
  main entry point to run the tests.
- ``value :: NFData a => String -> a -> Weigh ()``: Measure the memory
  allocations of a single value.
- ``func :: NFData a => String -> (b -> a) -> b -> Weigh ()``: Measure the memory
  allocations that result from the input function.
- ``wgroup :: String -> Weigh () -> Weigh ()``: Define a group of tests.
- ``io :: NFData a => String -> (b -> IO a) -> b -> Weigh ()``: Weigh an IO
  action that is applied to the input argument ``b``.
- ``io :: NFData a => String -> IO a -> Weigh ()``: Weigh an IO action.

We recommend using ``func`` over ``value`` because GHC might float out a given
value and thus not perform any allocations. For example, consider this program:

.. code-block:: haskell

  data Foo0 = Foo0
    deriving (Generic,NFData)

  data Foo1 = Foo1 Int
    deriving (Generic,NFData)

  data Foo2 = Foo2 String String
    deriving (Generic,NFData)

  one,two :: String
  one = "one"
  two = "two"

  main :: IO ()
  main = mainWith $ do
    value "()" ()
    value "1"  (1 :: Int)
    value "True"  True
    value "[0..3]"  ([0..3] :: [Int])
    value "[0,1,2,3]"  ([0,1,2,3] :: [Int])
    value "Foo0"  Foo0
    func  "Foo1-func"  Foo1 1
    value "Foo1-value"  (Foo1 1)
    value "one" one
    value "Foo2"  (Foo2 one two)


One might expect ``()``, ``1``, and ``True`` to be 0 machine words, 2 machine
words and 0 words respectively. However, this is not the case; here is the
output from weigh:

.. code-block:: bash

   Running 1 benchmarks...
   Benchmark weigh: RUNNING...

   Case        Allocated  GCs
   ()                  0    0
   1                   0    0
   True                0    0
   [0..3]            192    0
   [0,1,2,3]           0    0
   Foo0                0    0
   Foo1-func          16    0
   Foo1-value          0    0
   one               144    0
   Foo2              336    0
   Benchmark weigh: FINISH

A word of caution, some results might be puzzling at first: notice that built in
types such as ``()`` and ``True`` do not do any allocation. This is because
these types are `wired-in
<https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/wired-in>`_ to
GHC, which means that there is a single shared ``()`` in GHC and thus our call
to ``value "()" ()`` performs no allocation because it references the shared
``()``. This is also true for ``True``. ``1`` performs no allocation *during the
runtime* of our program because GHC realizes its a static literal and floats it
out; and similarly so for ``[0,1,2,3]`` and ``Foo0`` . In contrast, the list
ranges ``[0..3]`` and ``([0,1,2],[3,4,5])`` do perform allocation during
runtime. ``Foo1-func`` allocates because we used ``func`` which forces the
creation of ``Foo1`` at runtime, in contrast ``Foo1-value`` performs no
allocation and was likely optimized by GHC. Lastly, ``Foo2`` allocates because
string literals and a ``Foo2`` value is constructed at runtime.


Examples
--------


Summary
-------

References and Further Reading
------------------------------

#. The FPComplete `blog post
   <https://www.fpcomplete.com/blog/2016/05/weigh-package/>`_ on weigh
