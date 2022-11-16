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
3.

Weigh works by tracking the allocation and garbage collection behavior of the
runtime system, but it does so by taking snapshots before and after forcing
whatever value you are passing to it. Thus, it will report incorrect
measurements if another thread changes the heap unexpectedly. Similarly, it will
report larger results if the values are artificially inflated for profiling.


What Information Do I Receive From Weigh?
--------------------------------------------

Weigh reports a table

When should I use Weigh
-----------------------

How should I use Weigh
----------------------

Examples
--------

Summary
-------

References and Further Reading
------------------------------
