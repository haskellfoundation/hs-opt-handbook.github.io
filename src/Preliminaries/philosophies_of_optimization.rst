.. _Philosophies of Optimization Chapter:

Philosophies of Optimization
============================

What does it mean to optimize a code base? What is optimization? If we do not
understand what it means to optimize, then how can we possibly optimize a code
base? In this chapter, we present philosophies of optimization to answers these
questions. These ideas hail from the performance oriented culture of video game
developers [#]_, we've only summarized them here.

There are three philosophies of optimization, they are:

.. _Optimization:

Optimization
------------

This is actual optimization, the idea is to change the implementation to target
a *specific* machine or kind of machine. The following tasks are optimization
tasks:

a. Checking the specifications of the machine to determine how fast the machine
   can do the mathematical operations the workload and program implementation
   require.

b. Inspecting the operations the program is required to do to ensure they are
   optimal for the machine the implementation will run on. For example, altering
   the implementation to avoid specific assembly instructions such as ``jmp``
   and instead generating ``cmov``.

c. Ensuring that the algorithm the code implements is optimal for the
   workload.

d. Benchmarking the implementation and comparing the results to the theoretical
   maximum the machine is capable of, and then inspecting the implementation's
   runtime to determine where exactly the program slows down.

An example of optimization in Haskell would be tuning the runtime system flags
to the machine the program will run on. For example, building the program with
no-tables-next-to-code because we have measured that tables-next-to-code
:ref:`increases L1 cache-misses <Checking the L1 Cache>` for the machine we
intend to run on production.

Actual optimization is hard and time consuming. There is a time and place for
it, but it should not be the bulk of your optimization work in normal
circumstances because its benefits are overly specific to one kind of machine.
So in the general case, where you are writing software that runs on machines you
don't know anything about, you should instead optimize via non-pessimization.

.. _Non-Pessimization:

Non-Pessimization
-----------------

Non-pessimization is a philosophy of crafting software where one tries to write
software that does the least amount of work possible. Or in other words, this
philosophy asks us to write code that minimizes extra work the CPU must do. The
idea behind non-pessimization is that modern hyperscaler pipelining CPUs are
extremely fast, and by *not* burdening the CPU with extra work, the
implementation will necessarily be performant.

A typical example of pessimized code that Haskellers' should be familiar with is
an excessive use of laziness for a workload that simply does not require the
laziness. For example, computing the sum of an input list with a lazy
accumulator. This is an example of pessimized code because the code is
requesting the CPU do extra non-necessary work. That work being the allocating
thunks, and then searching for thunks distributed all about the heap. Of course
each thunk will and must be eventually scrutinized, but conceptually the
workload does not benefit from and does not require laziness. Thus the
construction and eventual scrutinization of these thunks is simply wasted time
and effort placed onto the CPU.

Key to this approach is keeping in mind what the machine must do in order to
complete the work load that your program defines. Once you have grokked this
thinking, writing code that does the least amount of work will follow. In the
previous example of lazy accumulation, the author of that code was not thinking
in terms of the machine. Had they been thinking in terms of the operations the
machine must perform, then they would have observed that the thunks were
superfluous to the requisite workload.

Some more examples of pessimized code are:

a. Too much polymorphism and higher ordered functions. In general, anything that
   could add an :term:`Unknown Function` to hot loops that we care about is, and
   will be unnecessary work for the CPU.

b. Using lot's of libraries with code that you do not understand and have not
   benchmarked. Libraries will prioritize whatever the library author felt was
   important. Note that If one of those things is performance, and you find (by
   empirically measuring) that the library is suitably performant for your
   workload then by all means use it. The point being that you should be
   deliberate and selective with your dependencies and should empirically assess
   them.

c. Excessive use of Constructors and fancy types [#]_. For non-pessimized code
   we want to do *as little* as possible. This certainly means avoiding the
   creation of a lot of objects that live all over the heap.

d. Defining types with poor memory efficiency. Consider this example from
   GHC's STG implementation:

   .. code-block:: haskell

      data LambdaFormInfo
      =
      ...
      | LFThunk             -- Thunk (zero arity)
              !TopLevelFlag
              !Bool           -- True <=> no free vars
              !Bool           -- True <=> updatable (i.e., *not* single-entry)
              !StandardFormInfo
              !Bool           -- True <=> *might* be a function type
      ...

The constructor ``LFThunk`` has five fields, three of which are ``Bool``. This
means, in the abstract, that we only need three bits to store the information
that these ``Bool``'s represent. Yet in this constructor each ``Bool`` will be
padded by GHC to a machine word. Therefore, *each* ``Bool`` is represented with
64-bits on a typical x86_64 machine (32-bits for x86 and for other backends such
as the JavaScript backend). Thus, one ``LFThunk`` heap object will require 320
bits (192 bits for the ``Bool``'s, 128 for the other two fields), of which 188
bits will always be zero because they are wasted space. Similarly,
``TopLevelFlag`` is isomorphic to a ``Bool``:

.. code-block:: haskell

   data TopLevelFlag
   = TopLevel
   | NotTopLevel
   deriving Data

So a more efficient representation *only requires* 4 bits and then a pointer to
``StandardFormInfo`` for a total of 66 bits. However, this must still be aligned
and padded; yielding a total of 72 bits, which is a 77% improvement in memory
efficiency.

Non-pessimization should be the bulk of your optimization efforts. Not only is
it portable to other machines, but it is also simpler and more future proof than
actual optimization.

.. _Fake Optimization:

Fake Optimization
-----------------

Fake optimization is a philosophy of performance that will not lead to better
code or better performance. Rather, fake optimization is advice that one finds
around the internet. These are sayings such as "You should never use <Foo>!", or
"Google doesn't use <Bar> therefore you shouldn't either!", or "you should
always use arrays and never use linked-lists". Notice that each of these
statements are categorical; they claim something is *always* fast or slow or one
should *never* or *always* use something or other.

These statements are called fake optimizations because they are advice or
aphorisms that are divorced from the context of your code, the problem your code
wants to solve and the work it must perform to do so. An algorithm or data
structure is not *universally* bad or good, or fast or slow. It could be the
case that for a particular workload, and for a particular memory access pattern,
a linked-list is the right choice. The key point is that whether an algorithm or
data structure is fast or not depends on numerous factors. Factors such as what
your program has to do, what the properties of the data your program is
processing are, and what the memory access patterns are. Another example of a
fake optimization statement is "quick-sort is always faster than
insertion sort". This is a fake optimization because while quick-sort has better
time complexity than insertion sort, for small lists (usually less than 30
elements) insertion sort will be more performant [#]_.

The key idea is that the performance of your code is very sensitive to the
specific problem and data the code operates on. So beware of fake optimization
statements for they will waste your time and iteration cycles.


References and Footnotes
------------------------

.. [#] See `this <https://youtu.be/pgoetgxecw8?si=0csotFBkya5gGDvJ>`__ series by
       Casey Muratori. We thank him for his labor.

.. [#] I hear you say "but this is Haskell!" why wouldn't I use algebraic data
       types to model my domain and increase the correctness and maintainability
       of my code! And you are correct to feel this way, but in this domain, we
       are looking for performance at the expense of these other properties and
       in this pursuit you should be prepared to kill your darlings. This does
       not mean you must start rewriting your entire code base. Far from it, in
       practice you should only need to non-pessimize certain high-performance
       subsystems in your code base. So it is key that one practices writing
       non-pessimized Haskell such that when the need arises you understand how
       to speed up some subsystem by employing non-pessimizing techniques.

.. [#] See this `keynote <https://youtu.be/FJJTYQYB1JQ?si=L2pDU5AqFNjFC1hK>`__
       by Andrei Alexandrescu. Another example is `timsort
       <https://en.wikipedia.org/wiki/Timsort>`__ in Python. Python `adopted
       <https://mail.python.org/pipermail/python-dev/2002-July/026837.html>`__
       timsort because most real-world data is nearly sorted, thus the
       worst-case *in practice* is vanishingly rare.
