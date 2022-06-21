.. _sec-lethargy:

The Programs of Consistent Lethargy
============================================

We'll begin by showing small bite sized programs that demonstrate a particular
way Haskell programs slow down. We call these programs *canonical* programs
because each program is the smallest example of a kind of slow down. A reader
should come away from this section with an understanding of the ways a
Haskell program slows down. For each slow down topic we provide a sister



.. _canonical-inlining:

Inlining
--------

What is Inlining
^^^^^^^^^^^^^^^^

Inlining [#]_ is a simple optimization technique that almost all optimizing
compilers perform. The essential idea is to substitute the call sites of a
function ``f``, with the body of ``f``. For example:

.. code-block:: haskell
   :caption: Before inlining

   > let f x = x * 3
   --- somewhere else
   > f (a + b) - c

Here we define a function ``f``, and then have a single call site ``f (a + b)
...``. Inlining ``f`` transforms the call site by replacing ``f (a + b)``
with the body (or right hand side) of ``f``:

.. code-block:: haskell
   :caption: After inlining

   > let f x = x * 3
   -- somewhere else
   > (a + b) * 3 - c

Notice the call to ``f`` is removed and has been replaced with ``x * 3``, where
:math:`x \mapsto (a + b)`.


Why do we want Inlining
^^^^^^^^^^^^^^^^^^^^^^^

Inlining has been called the "mother of all optimizations" because it has two
primary benefits. First, it removes the overhead of a function call, which can
be noticeable in a hot loop. Second, it is an *enabling optimization*; by
substituting the body of a function at its call sites, more optimizations are
possible on the inlined result rather than the non-inlined result, thus leading
to faster code. This is one of the main reasons why performance engineering is
more art than science; a simple change can have cascading and unforeseen effects
in the end result.

How does Inlining slow down runtime performance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Inlining itself does not slow down runtime performance, *lack of* inlining does,
because it limits otherwise possible optimizations from taking place. However,
that does not mean we should always ask GHC to inline or manually perform
inlining, in contrast, sometimes we can realize performance benefits by
restricting inlining. We'll return to the cost benefit analysis, and discuss the
particulars of GHC's inliner, in the chapter dedicated to :ref:`Inlining`.



.. _canonical-fusion:

Fusion
------

What is Fusion
^^^^^^^^^^^^^^

Fusion or Deforestation [#]_ [#]_ (We take Andy Gill's excellent examples from
this paper for this section. Thank you Andy Gill for your labor!) is an
optimization technique that eliminates intermediate and short lived data
structures between function calls. It is a key optimization that makes Haskell
fast because idiomatic Haskell style is often written in compositional style.
For example:

.. code-block:: haskell

   all p xs = and (map p xs)

In ``all`` we apply ``p`` to each element of ``xs`` in ``map p xs`` , this
produces an intermediate list of Booleans. This list is then consumed by ``and``
to produce the final single ``Bool`` result. Unfortunately, the allocation and
collection of the intermediate list is expensive; requiring each ``Cons`` cell
to be allocated on the heap, filled with a thunk, reified to a value, consumed
by ``and`` and then finally deallocated. This costs extra CPU cycles and places
more pressure on the garbage collector.

Fusion is an optimization technique that transforms functions such as ``all``
into versions that do not use intermediate lists:

.. code-block:: haskell

   all' p xs = h xs
     where h []     = True
           h (x:xs) = p x && h xs

In this version of ``all`` we *do not* allocate any intermediate list. Instead,
we create each Boolean value and immediately pass that value to ``&&`` creating
a chain of function calls (in fact, the exact chain of function calls that would
occur using ``and``!). We say that ``and`` and ``map`` have *fused*, because
this version is successful in removing the intermediate List, .

Why do we want Fusion
^^^^^^^^^^^^^^^^^^^^^^^

As Andy Gill writes:

   We want to eat our cake and have it too. That is, we would like to write
   programs in the style of ``all`` but have the compiler automatically
   transform this into the more efficient version ``all'``.

How does Fusion slow down runtime performance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Similar to Inlining, fusion itself does not slow down performance, rather *lack
of* fusion does, because if something can fuse but doesn't, then the program
will perform unnecessary allocations just to yield the same result. The
difficult parts of a fusion slow down is identifying fusion as the root cause of
your slow down *and then* convincing GHC to fuse whichever code was being
difficult. We'll show how to identify fusion as the culprit and convice GHC to
fuse in the chapter dedicated :ref:`Fusion`.




.. _canonical-pointer-chasing:

Excessive Pointer Chasing
-------------------------

Excessive pointer chasing is the enemy of any high performance Haskell program,
and presents itself in various forms; most of which Haskeller's are familiar
with to some degree. These include memory leaks during folds (strict and lazy),
for example:

.. code-block:: haskell

   mean :: [Double] -> Double
   mean xs =




using Boxed fields in data constructor definitions,


.. _canonical-closure-alloc:

Excessive Closure Allocation
----------------------------

.. _canonical-domain-modeling:

Poor Domain Modeling
--------------------


References
----------
.. [#] https://wiki.haskell.org/Inlining_and_Specialisation
.. [#] https://www.sciencedirect.com/science/article/pii/030439759090147A?via%3Dihub
.. [#] https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/deforestation-short-cut.pdf
