.. _EventLog Chapter:

..
   Local Variables
.. |eventlog2html| replace:: `eventlog2html <https://mpickering.github.io/eventlog2html/>`__


Eventlog
========

`Eventlog
<https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html#rts-eventlog>`_
is a GHC runtime system feature that logs events at runtime of a compiled
program. It is able to provide profiling information on the heap, the garbage
collector, the scheduler, and arbitrary events that a user inserts into their
own code. Eventlog is a versatile profiling tool, and should be one of the first
tools in the profiling toolbox you reach for. Use eventlog when you are just
beginning to diagnose the problem and are gathering data and want to inspect the
heap, or, if you already suspect a sub-system, such as the garbage collector,
and want to visualize its behavior. This chapter walks through using eventlog to
inspect a small program that suffers from :ref:`Excessive Closure Allocation`.
By the end of the chapter you should understand:

#. What information can you retrieve by using eventlog.
#. How to build your program to use eventlog.
#. How to visualize eventlog information.
#. How to tune eventlog to inspect specific subsystems.
#. How to tune eventlog to inspect specific pieces of code.
#. When to use eventlog.

Requirements
------------

#. The program must be recompiled with the ``-eventlog`` GHC flag
#. A program to consume the ``<program>.eventlog`` file. We recommend
   |eventlog2html|; see also the relevant section in the :userGuide:`GHC User's
   Guide <runtime_control.html#rts-eventlog>`. You can also parse the
   ``<program>.eventlog`` file using the `ghc-events
   <https://hackage.haskell.org/package/ghc-events>`_ library.


What Information Do I Receive From Eventlog?
--------------------------------------------

Eventlog logs events as a function of runtime. A full list of possible events is
available in :userGuide:`GHC User's Guide <runtime_control.html#rts-eventlog>`.
In general, the most common use case is to track heap events; which will be the
focus of this chapter. However, a user may define and track their own events
using the base functions `traceEvent
<https://downloads.haskell.org/~ghc/9.2.4/docs/html/libraries/base-4.16.3.0/Debug-Trace.html#v:traceMarker>`_
or `traceMarker
<https://downloads.haskell.org/~ghc/9.2.4/docs/html/libraries/base-4.16.3.0/Debug-Trace.html#v:traceMarker>`_
.

When should I use Eventlog
--------------------------

Eventlog is most useful when you need to :ref:`Characterize the Problem` . It
yields runtime information of the program specific to subsystems the program
relies on. Thus, it allows you to drill down into the behavior of the garbage
collector, the scheduler, the heap and so. For example, using the flag ``+RTS
-lg`` you can collect the ``CONC_MARK_BEGIN`` and ``CONC_MARK_END`` events which
log the beginning and end of the concurrent garbage collectors marking phase.
Similarly, you can collect ``MEM_RETURN`` which provides information about the
current allocation of megablocks, attempts to return them to the operating
system, and heap fragmentation.


The Running Example
-------------------

Our test program is an example of excessive pointer chasing. The toy example
should be familiar to most Haskeller's as a traditional example of a memory
leak:

.. code-block:: haskell

   {-# LANGUAGE BangPatterns #-}
   -- Need to disable optimizations because GHC will recognize and perform
   -- let-floating for us!
   {-# OPTIONS_GHC -O0 -ddump-simpl -ddump-to-file -ddump-stg-final #-}

   module Main where

   import Data.List              (foldl')
   import System.Random          (mkStdGen)
   import System.Random.Stateful (newIOGenM, uniformRM)
   import Control.Concurrent     (threadDelay)
   import Control.Monad          (replicateM)

   lazy_mean :: [Double] -> Double
   lazy_mean xs = s / fromIntegral ln
     where (s, ln)        = foldl step (0,0) xs
           step (s, ln) a = (s + a, ln + 1)

   stricter_mean :: [Double] -> Double
   stricter_mean xs = s / fromIntegral ln
     where (s, ln)        = foldl' step (0,0) xs
           step (s, ln) a = (s + a, ln + 1)

   strict_mean :: [Double] -> Double
   strict_mean xs = s / fromIntegral ln
     where (s, ln)        = foldl' step (0,0) xs
           step (!s, !ln) a = (s + a, ln + 1)

   main :: IO ()
   main = do
     -- generate random test data
     seed <- newIOGenM (mkStdGen 1729)
     test_values <- replicateM 500000 $ uniformRM (0,500000) seed
     -- sleep for a second
     let wait = threadDelay 1000000
     -- now run
     print $! lazy_mean test_values
     wait
     print $! stricter_mean test_values
     wait
     print $! strict_mean test_values

We define three functions, each of which calculate a geometric mean from a list
of Doubles. ``lazy_mean`` uses a lazy left fold, ``stricter_mean`` uses a strict
left fold but will still have a memory leak because ``foldl'`` evaluates the
operator to :term:`WHNF`. The operator in each fold is ``step`` whose WHNF is a
tuple constructor. Thus, even ``stricter_mean`` will leak memory because the
elements of the tuple *are still* lazy. ``strict_mean`` fixes this by adding
bang patterns *inside* the tuple, thereby forcing the elements to evaluate to
WHNF; which is just a value for ``Double``.

GHC is good at spotting such code patterns so we've turned off optimizations
with the ``OPTIONS_GHC -O0`` pragma. We use gauge (see :ref:`Criterion, Gauge,
and Tasty-Bench`) to measure the runtime of each function.


The Setup
---------

Using Eventlog requires three pieces of setup. First, you must build your
programs with the ``-eventlog -rtsopts -prof`` GHC flags (or alternatively set
``profiling: True`` in ``cabal.project`` or enable ``library-profiling`` and
``executable-profiling`` in ``stack.yaml``.). For example:

.. code-block::

   benchmark pointerChasing
     type            : exitcode-stdio-1.0
     default-language: Haskell2010
     ghc-options     : -fforce-recomp -threaded -rtsopts -prof -eventlog
     build-depends: base >= 4.15
                  , containers
                  , deepseq
                  , gauge
                  , random
     hs-source-dirs: bench/PointerChasing
     main-is: Main.hs

Second, you must pass the RTS flag ``-l`` to your program *and* additional RTS
flags that describe which events to track. Lastly, you must pass RTS flags to
describe the kind of heap information to collect. Here are some examples of RTS
flag combinations:

#. ``<program> +RTS -hy -l-agu -RTS``: Do not track all possible events
   (``-a``), but track all garbage collector events (``g``), all user events
   (``u``) and produce a heap profile by type (``-hy``).

#. ``<program> +RTS -hr -la -RTS``: Trace all possible events (``a``) and
   produce a heap profile by retainer (``-hr``).

#. ``<program> +RTS -hb -l-asu -RTS``: Do not track all possible events
   (``-a``), but track all scheduler events (``s``), all user events (``u``) and
   produce a heap profile by biography (``-hb``).

Heap Profile by Type
--------------------

To view the heap profile we'll use |eventlog2html|. To begin we'll inspect the
heap by type. Our initial goal is to determine if we have a memory leak and if
so which type is leaking. Here is the cabal file entry and invocation:

.. note::

   For subsequent runs, we will elide the complete output

.. code-block:: bash
   :caption: Run the benchmark, generate an eventlog of only (-a) user (u) and
             GC (g) events with a heap profile by type (-hy)

   $ cabal bench pointerChasing --benchmark-options='+RTS -hy -l-agu -RTS'
   Build profile: -w ghc-9.2.4 -O1
   In order, the following will be built (use -v for more details):
    - lethargy-0.1.0.0 (bench:pointerChasing) (first run)
   Preprocessing benchmark 'pointerChasing' for lethargy-0.1.0.0..
   Building benchmark 'pointerChasing' for lethargy-0.1.0.0..
   [1 of 1] Compiling Main             ( bench/PointerChasing/Main.hs, /home/doyougnu/writing/iohk/hs-opt-handbook.github.io/code/lethargy/dist-newstyle/build/x86_64-linux/ghc-9.2.4/lethargy-0.1.0.0/b/pointerChasing/build/pointerChasing/pointerChasing-tmp/Main.o )
   [1 of 1] Compiling Main             ( bench/PointerChasing/Main.hs, /home/doyougnu/writing/iohk/hs-opt-handbook.github.io/code/lethargy/dist-newstyle/build/x86_64-linux/ghc-9.2.4/lethargy-0.1.0.0/b/pointerChasing/build/pointerChasing/pointerChasing-tmp/Main.o )
   Linking /home/doyougnu/writing/iohk/hs-opt-handbook.github.io/code/lethargy/dist-newstyle/build/x86_64-linux/ghc-9.2.4/lethargy-0.1.0.0/b/pointerChasing/build/pointerChasing/pointerChasing ...
   Running 1 benchmarks...
   Benchmark pointerChasing: RUNNING...
   250137.43193906464
   250137.43193906464
   250137.43193906464
   Benchmark pointerChasing: FINISH

   $ eventlog2html pointerChasing.eventlog

   $ firefox pointerChasing.eventlog.html

which produces the heap profile:

.. image:: /_images/Measurement_Observation/Heap_GHC/eventlog/pc_heap_type.svg

This heap profile is a classic case of a memory leak because it is shaped like a
triangle. It is triangular because ``lazy_mean`` builds up a lot of thunks;
increasing allocations on the heap and producing the rising edge, the program
reaches a point where the thunks must be evaluated; producing the top of the
triangle, and then begins evaluating them thus decreasing the allocations on the
heap and which yields the descending edge.

The advantage of eventlog2html over :ref:`traditional tools <GHC Flags>` is its
interactivity and detailed heap breakdown.

Tuning the Output
-----------------

Summary
-------

References and Further Reading
------------------------------
#. The
