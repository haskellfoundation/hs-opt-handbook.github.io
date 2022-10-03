.. _EventLog Chapter:

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

What Information Do I Receive From Eventlog?
--------------------------------------------

Building with EventLog support
------------------------------

Visualizing EventLog
--------------------

Tuning the Output
-----------------

Summary
-------

References
----------
