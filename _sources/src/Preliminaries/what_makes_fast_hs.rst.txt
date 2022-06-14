.. _sec-lethargy:

The Programs of Consistent Lethargy
============================================

.. todo::
   - High level easy to understand enumeration of enemies of speed.
   - Start with abstract and generalities of slow programs in general, e.g., link chasing
   - Then move to haskell in particular

We'll begin by showing small bite sized programs that demonstrate a particular
way Haskell programs slow down. We call these programs *canonical* programs
because each program is a micro example of a kind of slow down without any extra
context, such as business logic. A reader should come away from this section
with an understanding of the central ways a Haskell program slows down.


Inlining
--------

What is Inlining
^^^^^^^^^^^^^^^^

Why do we want Inlining
^^^^^^^^^^^^^^^^^^^^^^^

How does Inlining slow down runtime performance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

How do I fix performance if Inlining is the issue
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


Lack of Fusion
--------------

What is Fusion
^^^^^^^^^^^^^^^^

Why do we want Fusion
^^^^^^^^^^^^^^^^^^^^^^^

How does Fusion slow down runtime performance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

How do I fix performance if Fusion is the issue
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


Excessive Pointer Chasing
-------------------------


Excessive Closure Allocation
----------------------------


Poor Domain Modeling
--------------------


References
----------
