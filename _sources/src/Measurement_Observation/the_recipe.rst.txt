
The Recipe
==========

This chapter presents a recipe for debugging performance regressions in Haskell.
Often times when we debug code it becomes too easy to trace execution or use a
shotgun approach; we apply a bunch of best-guess changes and retest to see if
our stimulus presents a response. You should do your best to avoid these urges.
Instead, use a scientific approach and develop a hypothesis and conceptual model
of the failure mode or bug. Every bug or performance regression is a learning
opportunity and should be considered as such. By treating regressions as
learning opportunities you gain knowledge of your system and the systems it
interacts with, and in turn become a better software engineer. This chapter
provides a sequence of questions and reminders to help you take a scientific
approach to performance regression debugging. We hope it aids you well.

Vocabulary
----------

Unless otherwise noted, we use the following vocabulary to describe various
aspects of our optimization journey. Because these do not have a formal
definition we present them here instead of in the :ref:`glossary`:

1. *The system*: The system is the local infrastructure and computational
   edifice your program operates in. This includes your operating system, your
   CPU, your memory controller.

2. *The program*: The program is the program we are trying to optimize that runs
   on the system.

3. *The problem*: The problem is an observable phenomena of the program. It is
   the performance regression we are trying to characterize, understand, fix and
   prevent.

4. *The failure mode*: The failure mode is the sequence of interactions between
   sub-systems or external systems and your system that manifest the problem.

5. *The baseline*: The baseline is the observable, measurable behavior of the
   program which constitutes *normal operation*. This is how you know you have a
   problem.


Characterize the problem
------------------------

The first step to solving any kind of problem is characterization. The goal of
this step is to observe how the problem *presents* itself in the system in terms
of the sub-systems the system uses. No phenomena exists without leaving a trail
of evidence, and our purpose in this step is to find this trail and re-state the
problem description *in terms* of the system. You should begin by asking
yourself the following questions:

#. Have I observed this problem before? Is it a known failure mode? If so, then
   why does it continue to happen? What are the chain of events that have caused
   it to occur again?

#. Is the problem deterministic? Or is it stochastic? If it is stochastic what
   is the rate at which we observe the problem phenomena?

#. Is there anything *unique* about the environment the problem manifest in?
   Specifically:
   -. Does it manifest only on unique hardware?
   -. Does it manifest at a particular time of day or only on one person's machine?
   -. Does it manifest only when other processes are running?

#. Is the problem temporal? Has anything *recently* changed in the system or the
   environment the system operates in that could induce the problem? Bring out
   the git logs and begin bisecting!

Precisely Restate the Problem
"""""""""""""""""""""""""""""

With these questions in mind, the next step is to gather data [#]_ . You want to
gather enough data to be able to precisely describe the problem in terms of
metrics your system cares about. For example, in a high performance application
our system likely cares about any decrease in instructions per cycle (IPC) of
our CPU. So IPC is a useful metric to measure, in order to characterize the
problem by comparing the IPC on the problem input to the IPC of normal
operation. Other examples might be an increase in total CPU cycles, runtime
memory cost, missed branches, cache writes/reads misses, network requests,
dropped packets etc. Whatever the metric, the key point is that the problem *can
be observed* through a change in the metric compared to baseline.

Next we require a good description of the problem. "The system slows to a crawl"
is not a good description of the problem because it is too vague. How does the
system slow? At what point does it slow? On what input does it slow? Does this
always happen or is it a random occurrence? "The system consumes 36Gb out of
32Gb of memory on input ``foo``, after 4 seconds." is a good description of the
problem. It is a precise description; we know that whatever the root cause is,
it affects the runtime memory of our program and that the problem manifests at a
certain point during runtime. Additionally, this is a good description because
gives an estimate of the magnitude of the problem *in terms* of a metric our
system cares about, e.g., runtime memory, and because it identifies a sub-system
(the heap) in the failure mode.

This step is concluded when you are able to precisely restate the problem in
terms of metrics that are meaningful to your system.

Construct a Hypothetical Failure Mode
-------------------------------------

The next step is to construct a hypothetical failure mode that could produce the
observed problem. The failure mode should be a statement of sub-system
interactions.  Imagine walking through the system with
the data. At each step write down a description of events that happen to the
data and where the events occurred. For example, here is a possible failure mode
for high amounts of heap use:

  The system receives input ``Foo`` from the command line. ``Foo`` is validated
  as legal by ``parseUserInput`` and marked as such in the type system with the
  ``Legal`` newtype. The worker function ``fib`` is applied to ``Legal Foo``.
  ``fib`` unpacks ``Legal Foo`` to receive a raw input of 1729. ``fib`` begins
  computing and we observe the problem.

Of course, this is a toy example and the failure mode in a large complex system
may be very long. If this is the case then begin writing the failure mode in
broad strokes, e.g.:

  Input ``Foo`` is input to the system, it then propogates to sub-system
  ``Bar``, is changed to ``FooFoo`` and then propogates to sub-system ``Baz``.

In this style you are not overly concerned with the exact functions which do the
work. Rather, you are simply laying out the path the problem input takes through
the system. You can fill in the details as you gain insight into the failure
mode through testing.

This step is concluded when you have identified and written down one or more
hypothetical failure modes.

Create the smallest reproducible test of the problem
----------------------------------------------------

Once you have characterized the problem and have possible failure modes you
should try to create an isolated, minimal test to reproduce the problem. Try to
construct the test such that it interacts with as few sub-systems and external
systems as possible to limit the scope of the investigation. At the end of the
investigation, you can add this test to your testsuite to ensure the problem
does not manifest again. If you have many possible failure modes, then try to
have one test per failure mode.

Creating a reproducible test is never the easy part, but it is not impossible.
To construct the test case try the following steps:

#. Try to isolate the sub-systems and external systems that you suspect are
   likely to be in the failure mode or failure modes.

#. Each external system provides information or a service to your system. Try to
   reproduce these dependencies in a deterministic way and treat them as inputs
   to your test case.

#. Try to isolate the code you believe to be in the failure mode. This should
   follow almost directly from characterizing the problem and defining the
   failure mode or modes. Tools such as valgrind, which provide line by line
   information of source code, are helpful here if CPU cycle counts are a
   meaningful metric for your system.

#. Remove all domain-specific information. Think of the possible failure mode
   from the perspective of the system. Do not think in terms of your business
   logic; using concepts such as ``Customer``, ``Bank Account``, or ``Payment
   Information``. Instead, think in terms of the realization of these concepts
   in your system. ``Customer`` is a ``String``, ``Bank Account`` is a
   ``Integer``, ``Payment information`` is a ``Text``. Now re-describe the
   possible failure mode in terms of the implementation concept: "When I send
   sub-system ``Foo`` a ``String`` that contains the character ``U+03BB`` I
   observe the problem".

#. Test different code paths to zero in on the failure mode. Run tests to see if
   you can deterministically observe the problem. You should be able to state
   "When I input ``Foo`` with properties ``Bar`` I observe the problem", and
   "When I input ``Baz`` with properties ``Qux`` I observe the baseline". You
   know you have found the right code path when you can reproducibly force the
   problem to occur *and* to not occur.


Define a hypothesis
-------------------

The objects of the hypothesis
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Think of each sub-system, external system, and component of your system as
characters in a story. Any system that takes an action to produce a result that
your code interacts with or causes, is a character. Each data structure your
code directly or indirectly uses, is a character. Each function you have
written, is a character; and so on. These are the objects of your hypothesis;
they are what the hypothesis makes a statement about, and define the sequence of
interactions that constitutes the failure mode.

Defining a good hypothesis
^^^^^^^^^^^^^^^^^^^^^^^^^^

Of course, not all hypotheses are equal. Good hypotheses have the following
properties:

#. They make progress; a good hypothesis yields information when confirmed *and*
   when invalidated. A bad hypothesis *keeps constant* the level of information
   you have about the phenomena. In other words, a bad hypothesis is one where
   you only gain information if the hypothesis is shown to true.

#. They have specificity and are actionable: Good hypotheses are specific enough
   *to be* invalidated. For example, the hypothesis "30% of CPU cycles are spent in
   ``Data.List.reverse`` on input ``Foo``" is actionable; we can directly measure
   how many CPU cycles are spent on this particular function for a particular
   input. But in addition to that, this hypothesis also adds information *even if*
   it is shown to be wrong. It could be the case that only 5% of the CPU cycles are
   spent on ``reverse`` and if so then we have still learned something.


Predict the Response and Test
-----------------------------

Now that you have your hypothesis and a minimal test case you can begin testing.
Each change made to your code should be in pursuit of validating or invalidating
the hypothesis. Do you best to resist the urge to begin shotgun debuggging! ..
[#].

Summary
-------


.. [#] Be sure to have a reproducible testing environment setup before you begin
       gathering data. :ref:`Repeatable Measurements`

.. [#] Shotgun debugging is usually an indication that you have not properly
       characterized the problem. The need to shotgun debug comes from not
       having identified the failure mode of the problem yet. In essence, when
       you shotgun debug, you add a bunch of stimulus into the system hoping for
       a response. If you get a response (the problem phenomena has changed)
       then you know you have stumbled upon the failure mode of the problem. If
       you do not get a response, then you know that the sub-systems you've
       altered are not in the failure mode of the problem. This search for the
       failure mode is characterization of the problem and thus so is shotgun
       debugging.
