.. _Weigh Chapter:

`Weigh`
=======

`Weigh <https://hackage.haskell.org/package/weigh>`_ is a tiny Haskell package
to measure allocations of data constructors and functions. It provides a similar
interface to :ref:`Criterion, Gauge, and Tasty-Bench <Tasty Chapter>` and is
useful to confirm that a data structure or function has the memory performance
you anticipate it to have at *runtime*. ``Weigh`` is easy to setup and
non-invasive; requiring no changes to source code. Thus, it is a good *initial*
tool to use before trying more advanced methods with higher setup costs, such as
:ref:`Cachegrind <Cachegrind>`.


Requirements
------------

1. The program must not be compiled with ``-threaded``.
2. The program must not be compiled with profiling enabled.

Weigh works by tracking the allocation and garbage collection behavior of the
runtime system. It takes snapshots before and after forcing whatever value or
function you are passing to it. Thus, it will report incorrect measurements if
another thread changes the heap unexpectedly. Similarly, it will report larger
results if the values are artificially inflated for profiling.

Restrictions
------------

Weigh is portable, and should work anywhere GHC's native runtime system will work.


What Information Do I Receive From Weigh?
--------------------------------------------

Weigh reports a table similar to ``criterion`` or ``gauge`` except displays
allocations in bytes and garbage collections by default. For example:

.. code-block:: bash

  Case       Allocated  GCs
  ()                 0    0
  1                  0    0
  True               0    0
  [0..3]           192    0

Notice that ``()``, ``1``, ``True`` do not allocate any memory. This is because
``weigh`` measures *heap* allocations. Thus, anything that GHC concludes can be
allocated at compile time, floated out of ``main`` and treated like a constant
will return 0 allocations. The list allocates due to the use of List Ranges.
Contrast that with a static literal version:

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
- Inspecting the memory footprint of a function. This information could inform
  the decision to manually apply certain GHC optimizations that might not be
  firing. Such as :ref:`Lambda Lifting <Lambda Lifting Chapter>` or the
  :ref:`SAT transformation <SAT Chapter>`.
- Inspecting the memory footprint of a data structure or a function under a
  *particular* load. This is useful information to tune the data structure
  specifically for that load. For example, one might make tradeoffs that create
  slower writes in exchange for faster reads if the load requires exponentially
  more reads than writes.
- Tracking the memory requirements and garbage collection pressure for phases or
  sub-systems of your program, or even defining unit tests based on the output
  of weigh.
- Inspecting the performance differences of the same code compiled with
  different versions of GHC.

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
value and statically allocate it. Thus the measurement will not observe the
allocation. For example, consider this program:

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
    value "()"         ()
    value "1"          (1 :: Int)
    value "True"       True
    value "[0..3]"     ([0..3] :: [Int])
    value "[0,1,2,3]"  ([0,1,2,3] :: [Int])
    value "Foo0"       Foo0
    func  "Foo1-func"  Foo1 1
    value "Foo1-value" (Foo1 1)
    value "one"        one
    value "Foo2"       (Foo2 one two)


One might :ref:`expect <Memory Footprint>` ``()``, ``1``, and ``True`` to be 0,
2 and 0 machine words respectively. However, this is not the case; here is the
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

Notice that built in types such as ``()`` and ``True`` do not do any allocation.
This is because these types are `wired-in
<https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/wired-in>`_ to
GHC, which means that there is a single shared ``()`` in GHC, so our call to
``value "()" ()`` performs no allocation because it references the shared
``()``. This is also true for ``True``. ``1`` performs no allocation *during the
runtime* of our program because GHC realizes it is a :term:`CAF` and float it
out of ``main``; and similarly so for ``[0,1,2,3]`` and ``Foo0`` . In contrast,
the list ranges ``[0..3]`` and ``([0,1,2],[3,4,5])`` do perform allocation
during runtime. ``Foo1-func`` allocates because we used ``func`` which forces
the creation of ``Foo1`` at runtime. ``Foo1-value`` performs no runtime
allocation because GHC will detect it as a CAF just like ``True``, ``()`` and
``1``. This is also why ``Foo2`` allocates; under the hood the ``Foo2`` value is
a ``CAF``, and its fields ``one`` and ``two`` are shared, but ``one`` and
``two`` are allocated at runtime via the ``unpackCString#`` primitive. Here is
the relevant :ref:`Core <Reading Core>`:

.. code-block:: haskell

   -- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
   Main.main_v2 :: Foo2
   [GblId,
    Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
            WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 10}]
   Main.main_v2 = Main.Foo2 one two


   -- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
   Main.one1 :: ghc-prim:GHC.Prim.Addr#
   [GblId,
    Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
            WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 20 0}]
   Main.one1 = "one"#

   -- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
   one :: String
   [GblId,
    Unf=Unf{Src=<vanilla>, TopLvl=True, Value=False, ConLike=True,
            WorkFree=False, Expandable=True, Guidance=IF_ARGS [] 20 0}]
   one = ghc-prim:GHC.CString.unpackCString# Main.one1

Notice that ``Main.Foo2`` calls ``one`` and ``two``, and that ``one`` is a CAF
because it has the properties ``Workfree=True``, ``Value=True``, and
``TopLvl=True``. But the payload of ``one``, ``Main.one1``, *is not* a CAF
because it is not a value and is not work free.

Examples
--------

Better Output By Setting Columns
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Weigh's default configuration is good enough, but we can query for more data
than just allocations and garbage collections using the function ``setColumns``:

.. code-block:: haskell

   main :: IO ()
   main = mainWith $ do
     setColumns [Case, Allocated, Max, Live, GCs, MaxOS] -- new
     value "()"         ()
     value "1"          (1 :: Int)
     value "True"       True
     value "[0..3]"     ([0..3] :: [Int])
     value "[0,1,2,3]"  ([0,1,2,3] :: [Int])
     value "Foo0"       Foo0
     func  "Foo1-func"  Foo1 1
     value "Foo1-value" (Foo1 1)
     value "one"        one
     value "Foo2"       (Foo2 one two)


which yields:

.. code-block:: bash

   Running 1 benchmarks...
   Benchmark weigh: RUNNING...

   Case        Allocated  Max  Live  GCs  MaxOS
   ()                  0  456   456    0      0
   1                   0  456   456    0      0
   True                0  456   456    0      0
   [0..3]            192  504   504    0      0
   [0,1,2,3]           0  456   456    0      0
   Foo0                0  456   456    0      0
   Foo1-func          16  472   472    0      0
   Foo1-value          0  456   456    0      0
   one               144  504   504    0      0
   Foo2              336  552   552    0      0
   Benchmark weigh: FINISH


and now we can see total bytes allocated, the maximum residency memory, the
total amount of live data on the heap, the number of garbage collections, and
the maximum memory in use by the RTS, which in these simple examples is
always 0.

Observe GHC Optimizations on your Data Type
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GHC is a good optimizing compiler and by using weigh we can investigate how GHC
is optimizing our program. Consider these data types:

.. code-block:: haskell

   data LotsOfInts = A Int Int
                   | B Int Int
                   | C Int Int
                   | D Int Int
                   | E Int Int
          deriving (Generic,NFData)

   data LotsOfInts2 = A2 Int Int
                    | B2 Int Int
                    | C2 Int Int
                    | D2 Int Int
                    | E2 Int Int
                    | F2 Int Int
                    | G2 Int Int
                    | H2 Int Int
             deriving (Generic,NFData)

With no optimizations taking place we would expect that a value of
``LotsOfInts`` requires 3 machine words: one for the data constructor header,
one for a pointer for each field. The payloads are simply ``Int`` which requires
two machine words each, one for the ``I#`` constructor and one for the payload
``Int#``. Note that we could directly use :term:`unboxed` Ints (that is store
``Int#`` instead of ``Int`` in ``LotsOfInts``), each constructor would still
require three words only instead of storing pointers we would be storing the
actual payload. Let's measure how many bytes a value of ``LotsOfInts`` and
``LotsOfInts2`` requires:

.. code-block:: haskell

   {-# OPTIONS_GHC -O2 #-}

   module Main where

   import Weigh

   data LotsOfInts = A Int Int
                   | B Int Int
                   | C Int Int
                   | D Int Int
                   | E Int Int
          deriving (Generic,NFData)

   data LotsOfInts2 = A2 Int Int
                    | B2 Int Int
                    | C2 Int Int
                    | D2 Int Int
                    | E2 Int Int
                    | F2 Int Int
                    | G2 Int Int
                    | H2 Int Int
                    | I2 Int Int
             deriving (Generic,NFData)

   -- notice that we'll use both value, and func to cover our bases
   main :: IO ()
   main = mainWith $ do
     setColumns [Case, Allocated, Max, Live, GCs, MaxOS]
     value "value: A Lot Of Ints"  (A 1000 1001)
     func  "func:  A Lot Of Ints"  (A 1000) 1001
     value "value: A Lot Of Ints2" (A2 1000 1001)
     func  "func:  A Lot Of Ints2" (A2 1000) 1001

which produces:

.. code-block::

   Benchmark weigh: RUNNING...

   Case                   Allocated  Max  Live  GCs  MaxOS
   value: A Lot Of Ints           0  456   456    0      0
   func:  A Lot Of Ints          24  480   480    0      0
   value: A Lot Of Ints2         72  456   456    0      0
   func:  A Lot Of Ints2         96  480   480    0      0
   Benchmark weigh: FINISH

We expected ``LotsOfInts`` to take a 3 machine words (or 24 bytes on a 64-bit
machine, 12 on a 32-bit machine) for a single value. However, we find that
instead of 24 bytes a new value allocates 0. In contrast, the function version
of ``LotsOfInts`` allocates 24 bytes. There are several things happening here:
First, ``A 1000 1001`` is recognized as a :term:`CAF` by GHC and allocated at
compiled time; which is why we do not see any allocation measured by weigh at
runtime. Second, GHC allocates 24 bytes for the function version because
partially applying ``(A 1000)`` to ``1001`` requires allocating ``1001`` (two
words) *and* a pointer to ``1001`` (one word).

We should expect that ``LotsOfInts`` and ``LotsOfInts2`` would have identical
memory characteristics because the only difference between them is the number of
constructors (five vs nine). We find that is not the case, instead
``LotsOfInts2`` allocates much more memory (3x!) than ``LotsOfInts``.
Furthermore, a value of ``LotsOfInts2`` allocates 9 words (72 bytes), which is 6
more words than expected *and* indicates that the value was not detected as a
CAF. What we're observing is the benefits of pointer-tagging optimizations (see
:cite:t:`pointerTaggingLaziness`). When a datatype has 8 or less (4 or less on
32-bit machines) constructors GHC uses the last 3-bits (2-bits on 32-bit) of the
constructor's pointer to store information about the constructor. This produces
a significant speedup and lowers memory costs in many ways: It allows the
runtime system to avoid checking the :term:`Info Table` for arity and payload
information, which improves branch prediction and function calls. It speeds up
case-expressions because the runtime system does not need to enter the scrutinee
to determine the constructor. Thus, it is a good idea to keep the number of
constructors for crucial datatypes at 8 or less.

We can double check that pointer-tagging is the culprit by removing a
constructor from ``LotsOfInts2``:

.. code-block:: haskell

   data LotsOfInts2 = A2 Int Int
                    | B2 Int Int
                    | C2 Int Int
                    | D2 Int Int
                    | E2 Int Int
                    | F2 Int Int
                    | G2 Int Int
                    | H2 Int Int
                    -- now we have exactly 8 constructors
                    -- | I2 Int Int
             deriving (Generic,NFData)

which produces:

.. code-block:: bash

   Benchmark weigh: RUNNING...

   Case                   Allocated  Max  Live  GCs  MaxOS
   value: A Lot Of Ints           0  456   456    0      0
   func:  A Lot Of Ints          24  480   480    0      0
   value: A Lot Of Ints2          0  456   456    0      0
   func:  A Lot Of Ints2         24  480   480    0      0
   Benchmark weigh: FINISH

And now ``LotsOfInts2`` behaves exactly like ``LotsOfInts``.

.. note::

   If you check the STG for the nine constructor version of ``LotsOfInts2``
   you'll find that the root of the allocations come from the ``Generic`` and
   ``NFData`` instances. The functions these instances generate use an extra
   case-expression to scrutinize a value of ``LotsOfInts2``, and in STG (and
   Core) that extra case expression means extra allocation. In addition to the
   extra case-expression, the nine constructor version disables :ref:`inlining
   <Inlining Chapter>` and :ref:`SAT <SAT Chapter>` optimizations.


Summary
-------

We have explored using the ``weigh`` library to do a shallow inspection of the
memory performance of a data type or function. Weigh is easy to setup and low
weight, so it a useful tool before reaching for more invasive methods. One
should use weigh to collect data for memory regressions, to verify mental models
of GHC's optimizations on data types in micro-benchmarks, or to inspect memory
behavior of a program under a particular load.

References and Further Reading
------------------------------

#. The FPComplete `blog post
   <https://www.fpcomplete.com/blog/2016/05/weigh-package/>`_ on weigh

Related Work
------------

#. `ghc-datasize <https://github.com/def-/ghc-datasize>`_
