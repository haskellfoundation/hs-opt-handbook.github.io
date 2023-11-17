.. _sec-lethargy:

The Programs of Consistent Lethargy
===================================

We'll begin by showing small bite sized programs that demonstrate a particular
way Haskell programs can slow down. We call these programs *canonical* programs
because each program is the smallest example of a kind of slow down. A reader
should come away from this section with an understanding of the ways a Haskell
program slows down. This chapter is just to small tour; for each slow down topic
we provide a sister chapter that explores the topic in more detail.

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


Why do We Want Inlining
^^^^^^^^^^^^^^^^^^^^^^^

Inlining has been called the "mother of all optimizations" because it has two
primary benefits. First, it removes the overhead of a function call, which can
be noticeable in a hot loop. Second, it is an *enabling optimization*; by
substituting the body of a function at its call sites, more optimizations are
possible on the inlined result rather than the non-inlined result, thus leading
to faster code. This is one of the main reasons why performance engineering is
more art than science; a simple change can have cascading and unforeseen effects
in the end result.

How does Inlining Slow Down Runtime Performance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Inlining itself does not slow down runtime performance, *lack of* inlining does,
because it limits otherwise possible optimizations from taking place. However,
that does not mean we should always ask GHC to inline or manually perform
inlining, in contrast, sometimes we can realize performance benefits by
restricting inlining. We'll return to the cost benefit analysis, and discuss the
particulars of GHC's inliner, in the chapter dedicated to :ref:`Inlining
<Inlining Chapter>`.


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
^^^^^^^^^^^^^^^^^^^^^

As Andy Gill writes:

   We want to eat our cake and have it too. That is, we would like to write
   programs in the style of ``all`` but have the compiler automatically
   transform this into the more efficient version ``all'``.

How does Fusion Slow Down Runtime Performance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Similar to Inlining, fusion itself does not slow down performance, rather *lack
of* fusion does, because if something can fuse but doesn't, then the program
will perform unnecessary allocations just to yield the same result. The
difficult parts of a fusion slow down is identifying fusion as the root cause of
your slow down *and then* convincing GHC to fuse whichever code was being
difficult. We'll show how to identify fusion as the culprit and convice GHC to
fuse in the chapter dedicated :ref:`Fusion <Fusion Rules Chapter>`.


.. _canonical-pointer-chasing:

Excessive Pointer Chasing
-------------------------

What is Excessive Pointer Chasing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Excessive pointer chasing is a form of superfluous computation; our program is
doing more work than it needs in order to compute the result. It occurs anytime
our programs de-reference a pointer to retrieve a value instead of just
referencing the value itself, thereby creating an extra layer of unnecessary
indirection. In Haskell programs this most often occurs when we write programs
without thinking about their memory representation; and especially around
laziness. As such, most of these instances are well known and have floated
around the community for some time.


How does Excessive Pointer Chasing Slow Down Runtime Performance?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The classic example of excessive pointer chasing is memory leaks that result
from folds that are overly lazy, for example [#]_:

.. code-block:: haskell
   :caption: mean, calculated with a lazy left fold

   mean :: [Double] -> Double
   mean xs = s / fromIntegral ln
     where (s, ln)        = foldl step (0,0) xs
           step (s, ln) a = (s + a, ln + 1)


.. code-block:: haskell
   :caption: mean, calculated with a strict left fold

   mean' :: [Double] -> Double
   mean' xs = s / fromIntegral ln
     where (s, ln)        = foldl' step (0,0) xs
           step (s, ln) a = (s + a, ln + 1)

.. note::
   `Never
   <https://github.com/hasura/graphql-engine/pull/2933#discussion_r328821960>`_
   use ``foldl`` on a list.

``mean`` and ``mean'`` are versions of a common source of memory leaks;
performing a fold that is *too lazy* over a data structure. Even ``mean'``,
which uses a strict left fold, leaks memory because ``foldl'`` is not strict
enough. ``foldl'`` evaluates its accumulator to :term:`WHNF`, in this case that
is a *lazy tuple* and so each call to ``step`` will only evaluate to the
constructor of the tuple: ``(,)``, *and will not* evaluate ``s + a`` or ``ln +
1``. These computations are stored as thunks on the heap, which will be pointed
to by the ``(,)`` constructor, and thus we have to chase these pointers to do
our computation.

Another form of common excessive pointer chasing is using lazy fields in a data
constructor that does not benefit from laziness and will be consumed anyway. For
example, consider the data type version of ``step``:

.. code-block:: haskell

   data Step = Step Double Double
   ...

   -- mean rewritten with Step instead of (,)
   mean'' :: [Double] -> Double
   mean'' xs = s / fromIntegral ln
     where (Step s  ln)       = foldl' step (0,0) xs
           step (Step s ln) a = Step (s + a) (ln + 1)

Just as ``mean'`` was excessively lazy, so will ``mean''`` be, because each
``Double`` in ``Step`` is lazy, and so both the ``(s + a)`` and ``(ln + 1)``
computations will be thunks. But in the domain of our program---calculating the
geometric average---we gain nothing from this laziness because our program
doesn't need to defer a computation. Instead, we would be better off immediately
consuming the intermediate ``Step`` values, and gaining performance by removing
the superfluous indirection.


A related form of common excessive pointer chasing is using :term:`Boxed` fields
in data constructors when :term:`Unboxed` fields would do. Consider an example
of a ``Counter`` data type that tracks some domain specific integer:

.. code-block:: haskell

   data Counter = Counter Int

.. note::
   Normally, when compiling with ``-O2`` GHC will recognize and optimize this definition.

``Int`` is a :term:`Boxed` and :term:`Lifted` type in ``Counter``, this means
that each ``Counter`` holds a pointer to an ``Int`` on the heap *not* a pointer
to an ``Int`` directly. We can instruct GHC remove the heap indirection with the
`unpack
<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/pragmas.html?highlight=unpack#unpack-pragma>`_
pragma and a bang pattern:

.. code-block:: haskell

   data Counter = Counter {-# UNPACK #-} !Int

This pragma instructs GHC to store the contents of ``Int`` directly in the
``Counter`` constructor, rather than storing a pointer to an ``Int`` on the heap
in the constructor. We'll return to these fixes in the :ref:`Unboxing` chapter.


.. _canonical-closure-alloc:

Excessive Closure Allocation
----------------------------

What is Excessive Closure Allocation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Excessive closure allocation is another form of superfluous computation and
superfluous memory allocation; it means that our program is doing more memory
allocation and likely more computation than required to compute the result.
Excessive closure allocation is subtle for two reasons: first, because GHC is
typically very good at optimizing it away via :term:`Let Floating` most
Haskeller's never have to confront it (which is a good indication of GHC's
quality); second, in order to observe it, the programmer must track the memory
allocation of their program across many functions, modules and packages, which
is not a common experience when writing Haskell. For our purposes', we'll
inspect examples that GHC should have no problem finding and optimizing. See the
:ref:`Impact of seq Removal on SBV's cache <SBV572>` case study for an example of excessive memory allocation in a widely used library. 

.. todo::
   Not yet written, see `#18 <https://github.com/input-output-hk/hs-opt-handbook.github.io/issues/18>`_

While
GHC is good at optimizing these cases, becoming familiar with these code
transformations is beneficial; it trains you to start thinking in terms of
memory allocation when reading or writing Haskell code, and teaches you to
perform these optimizations manually when GHC fails to optimize.


How does Excessive Closure Allocation Slow Down Runtime Performance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Consider these simple examples [#]_ :

.. code-block:: haskell

   let x = y + 1
   in case tail zs of
           [] -> x * x
           _  -> 1

This is an example of ``Let Floating inwards``. Notice that ``x`` is only used
in *one branch* of the ``case expression``, because the other branch does not
require it GHC can *Float x inward* to the first branch:

.. code-block:: haskell

   case tail zs of
        [] -> let x = y + 1
              in x * x
        _  -> 1

Now ``let x = ...`` occurs *conditionally* depending on the result of ``tail zs``,
rather than everytime as we saw in the first previous example. Thus, the second
form is semantically identical but more efficient because our program may avoid
``let x = ...`` and thereby avoid an extra heap allocation.

.. note::
   Let Floating can change :term:`Thunk` sizes.

Consider this example from :cite:t:`peytonjones1997a`, Section 7.1:

.. code-block:: haskell

   let x = v + w     -- v and w are free variables in x
       y = ...x...x  -- y mentions x
   in B              -- B does not mention x

Floating ``x`` inward produces:

.. code-block:: haskell

   let y = let x = v + w -- now v and w are free variables in y
           in ...x...x
   in B

Now ``v`` and ``w`` are free variables in ``y`` but ``x`` is not. ``x`` is a
bound variable in ``y`` (and will get inlined). So if ``v`` and ``w`` were
originally free in ``y`` then the size of the thunk for ``y`` will be unchanged.
However, if ``v`` and ``w`` are *newly* free in ``y`` then the size of the thunk
will increase to reference the new free variables.

Let bindings are also be floated outwards. There are several versions of outward
let floating which perform small optimizations by moving ``let`` bindings around
``case`` expressions, for now we'll focus on a very effective outward floating
transformation called the :term:`Full Laziness transformation`. The Full
Laziness transformation floats bindings out of lambda abstractions, consider:

.. code-block:: haskell

   f = \xs -> let
                g = \y -> let n = length xs  -- calculate n
                          in ...g...n        -- use n, but not xs
              in ...g...

So we have an outer function, ``f``, that defines a tight inner loop ``g``.
Notice that *every* recursive call to ``g`` will allocate space for, and
calclulate ``length xs`` because ``let n = ...`` is inside the body of ``g``,
and ``n`` is also used in ``g``. But this is clearly wasteful, ``xs`` isn't
changing in the body of ``g`` and so we should only need to calculate ``n``
once. Fortunately, ``g`` never uses ``xs`` other than to calculate ``n``, so
``let n = ...`` can be floated out of ``g``:

.. code-block:: haskell

   f = \xs -> let n = length xs          -- n only calculated once
              in let g = \y -> ...g...n  -- use previously defined n
                 in ...g...

This version is the full laziness version because we have moved ``let n = ..``
out of the lambda in the body of ``g``. This version is much more efficient by
utilizing laziness and avoiding repeated, wasteful computations of ``n``. ``n``
will be a thunk for the first iteration of ``g``, but for every other iteration
of ``g``, ``n`` will be evaluated to value thus saving time and space.

.. _canonical-domain-modeling:

Poor Domain Modeling
--------------------

What is Poor Domain Modeling
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Poor domain modeling is a catch all phrase for constructing a program that has a
high impedance to the problem domain. The problem domain is the abstract domain
that dictates the computation that the program must do, the logical sequence of
steps it must take and the invariants it must uphold. The program domain is the
implementation, it is the code that is tasked with performing the computation
and upholding the invariants in the problem domain. This is a one-to-many
relationship; for any given problem domain there are many possible
implementations.

For example, imagine our task is to implement a program that sorts some data. We
can list the concepts, invariants and properties this problem domain specifies:
the domain has the concepts of a datum; which is a single unit of information, a
partial order on that data; there are many sequences of data, but for a given
set of data only two sequences have the property sorted, a datum must have an
ordinal property; or else we would not be able to sort, and the sorted
invariant; that defines what the property sorted means: for a sort from low to
high, a given datum that is less than another datum must precede the greater
datum in the output sequence. Note that every possible implementation must
somehow represent and abide by these ideas for the program to be considered
correct and for the implementation to be considered an implementation at all.

Therefore poor domain modeling occurs when the implementation makes it difficult
to express the computation, properties and invariants required by the problem
domain. If this is the case then we say there is a high impedance between the
problem domain and the program domain. Obviously this is problem specific and we
cannot provide a canonical example, instead we'll provide a set of guidelines to
describe when you know you have high impedance and how to fix it.


How do I know if I have Poor Domain Modeling
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Unfortunately, this is more art than science. Classic indications in Haskell
are all instances of the implementation doing more work than is necessary:


Overuse of Data.List
""""""""""""""""""""

You've used a List and have called a function from ``Data.List`` that does any
kind of out-of-order processing on elements of the list, or must traverse the
entire list in order to produce a result:

#. ``length``
#. ``reverse``
#. ``splitAt``
#. ``takeWhile``
#. ``dropWhile``
#. ``elem``
#. ``notElem``
#. ``find``
#. ``filter``
#. any kind of indexing

Recall that lists in Haskell are streams; not treating them as such creates
impedance between the problem domain and your program in addition to degrading
runtime performance (and easily creating a quadratic time program). However,
small temporary lists holding single digits of elements are fine because they
take less time to construct and traverse than more complicated data structures.

Functions in your Program Domain do not Easily Compose to have Meaning in your Problem Domain
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

Composition and composability is one of the most valuable properties code can
have. It is key to modularity, key to reuse, creates code that is easier to
test, is easier to understand and is often more compact code. When the functions
in your program domain do not easily compose you'll often find yourself
constantly packing, unpacking, and repacking domain elements just to get
anything done. You'll be forced to reach into the *implementation* of objects in
your program domain in order to express meaning in your problem domain, rather
than expressing that meaning through functions.

When the program domain lacks composability, functions will become overly large
and overly concerned with implementation details; *that* is high impedance
expressing itself in the implementation.

.. todo::
   Need example as case study see `#20 <https://github.com/input-output-hk/hs-opt-handbook.github.io/issues/20>`_


Problem Domain Invariants are Difficult to Express
""""""""""""""""""""""""""""""""""""""""""""""""""

This one usually manifests through the use of superfluous guards. So many
functions take this form:

.. code-block:: haskell

   -- | an example function on Foo, this function learns a lot about Foo
   -- by testing Foo against many predicates
   myFunction :: Foo -> Bar
   myFunction foo | predicate0 foo = ...do something ...
                  | predicate1 foo = ...do another thing...
                  | ...
                  | predicateN foo = ...do N thing...

This becomes problematic when it grows to be ubiquitous in the code base.
When a lot of functions in the program use guards the program will suffer
from redundant checks and poor branch prediction, for example:

.. code-block:: haskell

   -- | another function on Foo, this function doesn't learn much about Foo
   -- because it only tests Foo against one predicate.
   myOtherFunction :: Foo -> Baz
   myOtherFunction foo | predicate1 foo = ...do some another thing...
                       | otherwise      = ...

   main :: IO ()
   main = do foo <- getFoo          -- we get a Foo
             myFunction foo         -- we learn a lot about Foo
             myOtherFunction foo    -- nothing we've learned is propagated forward
                                    --  from myFunction to myOtherFunction, and so
                                    --  we redundantly check predicate1 on foo.



References
----------
.. [#] https://wiki.haskell.org/Inlining_and_Specialisation
.. [#] https://www.sciencedirect.com/science/article/pii/030439759090147A?via%3Dihub
.. [#] https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/deforestation-short-cut.pdf
.. [#] This code adapted from Johan Tibell slides on Haskell `optimization
       <https://www.slideshare.net/tibbe/highperformance-haskell>`_.
.. [#] This code adapted from :cite:t:`peytonjones1997a` Section 7.
