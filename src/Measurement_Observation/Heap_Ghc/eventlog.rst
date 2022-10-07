.. _EventLog Chapter:

..
   Local Variables
.. |eventlog2html| replace:: `eventlog2html <https://github.com/mpickering/eventlog2html>`__


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
In general, the most common use case is to track heap events, however a user may
define and track their own events using the base functions `traceEvent
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

Our test program is an example of excessive closure allocation:

.. code-block:: haskell

    -- Need to disable optimizations because GHC will recognize and perform
    -- let-floating for us!
    {-# OPTIONS_GHC -O0 -ddump-simpl -ddump-to-file -ddump-stg-final #-}

    module Main where

    import Gauge

    -- | This function excessively allocates closures every time 'f' is called. The
    -- closure in question being the allocation of the 10k element list.
    bad :: [Int] -> Int
    bad xs = sum $ fmap f xs
      where f x = x + length [1..10000]

    -- | This function avoids the excessive closure allocation. We still will
    -- allocate thunks for every element of 'xs' but we only calculate 'length
    -- [1..10000]' once because we floated it out of 'f'.
    good :: [Int] -> Int
    good xs = sum $ fmap f xs
      where
        n = length [1..10000]
        f x = x + n

    -- | use Gauge to run the benchmarks
    main :: IO ()
    main = do
      let test_values = replicate 5000 1
      defaultMain [ bgroup "Too Many Closures" [ bench "bad"  $ whnf bad test_values
                                               , bench "good" $ whnf good test_values
                                               ]
                  ]

We define two functions ``good`` and ``bad``. ``bad`` has excessive closure
allocation because ``length [1..10000]`` is nested inside of ``f``. Thus the
list ``[1..10000]`` is repeatedly allocated, and ``length [1..10000]`` is
repeatedly computed for each call to ``f``. ``good`` floats ``length
[1..10000]`` out of ``f`` so the list and ``length`` application only occur
once.

.. note::
   The full laziness optimization will produce ``good`` from ``bad``.

GHC is good at spotting such code patterns so we've turned off optimizations
with the ``OPTIONS_GHC -O0`` pragma. We use gauge (see :ref:`Criterion, Gauge,
and Tasty-Bench`) to measure the runtime of each function.


The Setup
---------

Using Eventlog requires two pieces of setup. First, you must build your programs
with the ``-eventlog -rtsopts -prof`` GHC flags (or alternatively set
``profiling: True`` in ``cabal.project`` or enable ``library-profiling`` and
``executable-profiling`` in ``stack.yaml``.). Second, you must pass the RTS flag
``-l`` to your program *and* additional RTS flags that describe which events to
track. Here are some examples of RTS flag combinations:

#. ``<program> +RST -hy -l-agu``: Do not track all possible events (``a``), but
   track all garbage collector events (``g``) and all user events (``u``). This
   will produce an eventlog for heap profiling by types used in the program
   (``-hy``).


..
   Code example doesn't work but will for -prof so move to GHC flags chapter.
   Then we need to have a memory leak to show the heap with eventlog here

..
   heap
   profiling :ref:`GHC Flags` you wish, for example ``-hy`` or ``-hT`` for type and
   closure type, respectively.

Visualizing EventLog
--------------------

Tuning the Output
-----------------

Summary
-------

References and Further Reading
------------------------------
#. The
