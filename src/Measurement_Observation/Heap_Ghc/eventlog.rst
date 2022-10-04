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

#. What information can you retrieve by using eventlog
#. How to build your program to use eventlog
#. How to visualize eventlog information
#. How to tune eventlog to inspect specific subsystems
#. How to tune eventlog to inspect specific pieces of code
#. When to use eventlog

Requirements
------------

#. The program must be recompiled with the ``-eventlog`` GHC flag
#. A program to consume the ``program.eventlog`` file. We recommend
   |eventlog2html|; see also the relevant section in the :userGuide:`GHC User's
   Guide <runtime_control.html#rts-eventlog>`


What Information Do I Receive From Eventlog?
--------------------------------------------

Eventlog logs events as a function of time. For a full list of events see the
   `GHC User's Guide
   <https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html#rts-eventlog>`_.
   In general, the most common use case is to track heap events and arbitrary
   user events to get a heap profile.

When should I use Eventlog
--------------------------

Eventlog is most useful when you need to :ref:`Characterize the Problem`. With



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
with the ``OPTIONS_GHC -O0`` pragma.


Building with EventLog support
------------------------------

Visualizing EventLog
--------------------

Tuning the Output
-----------------

Summary
-------

References and Further Reading
------------------------------
