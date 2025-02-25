.. _The Checklist:

The Checklist
=============

Here is a checklist of things you might try to improve the performance of your
program.

- [ ] Are you compiling with ``-O2``, ``-Wall``?
- [ ] Have you checked for :ref:`memory leaks <Eventlog Chapter>` on the heap?
- [ ] Have you checked for :ref:`stack leaks <Reduced Stack>`?
- [ ] Have you :ref:`weighed <Weigh Chapter>` your data structures?
- [ ] Do you understand the :ref:`memory footprints <Memory Footprint of Data Types Chapter>` of your data types?
- [ ] Can you reduce the memory footprint of you data types?
- [ ] Are you using data structures that have a :ref:`low impedence <canonical-domain-modeling>` to your problem?
- [ ] Have you set up benchmarks? Are they realistic? Do the exercise the full data space?
- [ ] Are your data types strict? Can you unpack them?
- [ ] Have you removed excessive polymorphism?
- [ ] Are you using ``Text`` or ``ByteString`` instead of ``String``?
- [ ] Can you inline and monomorphise critical functions, especially in hot loops?
- [ ] Are you :ref:`accidentally allocating <excessive-closure-allocation>` in a hot loop?
- [ ] Are any functions in a hot loop taking more than five arguments? Can you reduce the number of arguments?
- [ ] Can you :ref:`defunctionalize <Defunctionalization Chapter>` critical functions? Is GHC defunctionalizing for you?
- [ ] Are you using a :ref:`left fold over a list <canonical-pointer-chasing>`?
- [ ] Are your datatypes definitions ordered such that the most common constructor is first?
- [ ] Are you using explicit export lists?
- [ ] Have you checked for :userGuide:`missed specializations </using-warnings.html#ghc-flag--Wmissed-specialisations>`?
- [ ] Have you checked the ratio of known to unknown function calls?
- [ ] Have you inspected the Core?
- [ ] Have you inspected the STG?
- [ ] Would your program benefit from compiling with ``LLVM``?
- [ ] Are you :ref:`shotgun parsing <shotgun-parsing>`? Can you lift information
  into the type system to avoid subsequent checks over the same data?
- [ ] Are you grouping things that need the same processing together?
- [ ] Could your program benefit for concurrency of parallelism?
- [ ] Could your program benefit from the :ref:`one-shot monad trick <OneShot Monad Chapter>`?
- [ ] Have you :ref:`unrolled <Unroll Monad Transformers Chapter>` your monad transformers?
- [ ] Have you inspected the :ref:`cache behavior <Cachegrind Chapter>`?

..
  The grouping things should be about data oriented design and using things like zigs arraylist

.. todo::
   Each item should have a concomitant link.

See also
--------

- `This older checklist <https://github.com/haskell-perf/checklist>`__.
