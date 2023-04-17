.. _How_to_use:

How to Use This Book
====================

Purpose
-------

This book is written as a handbook for the intrepid Haskell developer. It
attempts to make clear aspects of the GHC-based Haskell so that performance
optimizations and recommendations are actionable, rather than mysterious
invocations whispered around the internet.

Intended Audience
-----------------

The Haskell Optimization Handbook is intended for developers using Haskell in
their day to day life for money or pleasure. It assumes the audience is
proficient with pure functional programming concepts, such as recursion, purity,
higher-ordered functions functors, applicative functors, monads et cetera., and
the basics of the Haskell ecosystem, such as using cabal or stack to compile and
run a project.


What will I need?
-----------------

You need only code you are interested in optimizing and the will to go down the
optimization rabbit hole. If you have no code to optimize, then you may consider
picking your favorite Haskell library and attempting to optimize that!


The book assumes you are using GHC |ghcVersion| and a Linux distribution (kernel
version ``5.8`` and higher). Should you be using an older compiler than some
sections, such as :doc:`Using EventLog
</src/Measurement_Observation/Heap_Ghc/eventlog>`; which arrived in ``GHC 8.8``
may still be useful, while others such as :doc:`Using Cachegrind
</src/Measurement_Observation/Heap_Third/cachegrind>`; which relies on
:term:`DWARF` symbols (added in ``GHC 8.10.x``) may not be applicable.
Similarly, some chapters, such as :doc:`Using perf
</src/Measurement_Observation/Measurement_Third/linux_perf>` will only be
applicable for Linux and Linux based operating systems.

Notation
--------

Code examples are written in Haskell unless otherwise noted. Other notation
follows standards in programming language theory and computer science academic
discourse:

1. :math:`\perp` to mean the *bottom* value
2. :math:`Foo[x \rightarrow e]` to mean the result of substituting :math:`e` for
   :math:`x` in :math:`Foo`, and
3. Big-O notation: :math:`\mathcal{O}(n\log{}n)`, to mean the asymptotic running
   times of algorithms. In particular, if an algorithm takes
   :math:`\mathcal{O}(n \log{} n)` time, then it can process any set of
   :math:`n` input items in at most :math:`c*n \log{} n` time, for some fixed
   constant :math:`c`.

Where to Begin
--------------

The book is structured into discrete independent parts to better serve as a
handbook. Thus, the book is not meant to be read in a linear order. Instead, one
should pick and choose which chapter to read next based on their needs because
*the book assumes you have a problem that needs solving*.

There are two general sections; both are ordered from the least time consuming
to most time consuming topics. The first section, Part 1, aids the developer in
identifying performance issues in their own code. Part 1 is primarily concerned
with measurement, observation, repeatability and testing, but also includes
methods of *direct observation* such as inspecting and understanding the
``Core`` and ``Stg`` languages.

The second section, Part 2, aids the developer in optimizing their code. It is
ordered from the easiest methods, such as choosing the right libraries; to the
hardest methods, such as exploiting ``backpack`` for fine-grained
:term:`Unboxed` data types or exploiting :term:`Levity Polymorphism` to control
the runtime representation of a data type.


Goals
-----
HOH is:

#. A freely available online book.
#. Is actionable: A reader can read a section and apply a technique to make progress for their problem.
#. Is relevant: Case studies are not manufactured examples, rather they originate from real world code.
#. Is practical: Content that describes a performance technique describes how to
   implement it, when to use it, and its observable effect on a code base.
   Content that describes a method of analysis describes what the method is, how
   to use the method, what the perceived results of the method should be and
   relevant signals in the output of the method. For example, the GHC heap
   profiling section should describe what heap profiling is, how to do the heap
   profiling, what the output should look like, and most importantly a gallery
   of examples of poor performing heap profiles and an explanation of why they
   are poor performing.
#. Community driven.
#. Maintained and updated over time, with supported versions of GHC beginning at 8.10.x (for DWARF symbols).
#. Understandable for intermediate to expert Haskell developers.


Non-Goals
---------

HOH does not have:

#. Content suitable for beginner functional programmers.
#. Explanations of project setup, build tools, using or setting up a shell or
   deployment of any kind, instructions on installing any third party tools for
   a given platform.
#. Descriptions, analyses and explanations of functional algorithms or data
   structures. Content will instead be "Try unordered-containers if you have
   foo, bar, baz", rather than "This is what a bankers queue or HAMT is ...".
#. A monad or monad transformer tutorial. This is assumed knowledge in the
   audience.
