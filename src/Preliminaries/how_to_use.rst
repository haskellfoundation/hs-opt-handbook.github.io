.. _How to use this book

How to Use This Book
====================

Purpose
-------

This book is meant as a handbook for the intrepid Haskell developer. It attempts
to make clear aspects of the GHC-based Haskell so that performance optimizations
and recommendations are actionable, rather than mysterious invocations whispered
around the internet.


Notation
--------

Code examples are written in Haskell unless otherwise noted. Other notation
follows standards in programming language theory and computer science academic
discourse:

1. :math:`\perp` to mean the *bottom* value
2. :math:`Foo[x \rightarrow e]` to mean the result of substituting :math:`e` for
   :math:`x` in :math:`Foo`.
3. :math:`\mathcal{O}(n\log{}n)` to mean the minimum bound time complexity of an algorithm is :math:`n\log{}n`


Where to Begin
--------------

The book is structured into discrete independent parts to better serve as a
handbook. Thus, the book is not meant to be read in a linear order. Instead, one
should pick and choose which chapter to read next based on their needs because
*The book assumes you have a problem that needs solving*.

There are two general sections. The first section, Part 1, aids the developer in
identifying performance issues in their own code. As such, this section is
primarily concerned with measurement, observation, repeatability and testing.
Part 1 also includes methods of *direct observation* such as inspecting and
understanding the ``Core`` and ``Stg`` languages.

The second section, Part 2, aids the developer in optimizing their code. It is
ordered from the easiest methods, such as choosing the right libraries; to the
hardest methods, such as exploiting ``backpack`` for fine-grained
:term:`Unboxed` data types or exploiting :term:`Levity Polymorphism` to control the
runtime representation of a data type.

Goals
-----
HOH is:

#. A freely available online book
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
#. Community driven
#. Maintained and updated over time, with supported versions of GHC beginning at 8.10.x (for DWARF symbols)
#. Understandable for intermediate to expert Haskell developers.


Non-Goals
---------

HOH will not have:

#. Content suitable for beginner functional programmers.
#. Explanations of project setup, build tools, using or setting up a shell or
   deployment of any kind, instructions on installing any third party tools for
   a given platform.
#. Descriptions, analyses and explanations of functional algorithms or data
   structures. Content will instead be "Try unordered-containers if you have
   foo, bar, baz", rather than "This is what a bankers queue or HAMT is ...".
#. A monad or monad transformer tutorial. This is assumed knowledge in the
   audience.
