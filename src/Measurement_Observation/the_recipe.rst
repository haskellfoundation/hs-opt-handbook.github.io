
The Recipe
==========

This chapter presents a recipe for debugging performance regressions in Haskell.
This is a philosophical, not tool based chapter. Often times when we need to
debug code it becomes too easy to trace execution or use a shotgun approach;
where we apply a bunch of changes and retest to see if our stimulus presents a
response. You should do your best to avoid these urges. Instead, use a
scientific approach and develop a hypothesis and conceptual model of the failure
mode or bug. Every bug or performance regression is a learning opportunity and
should be considered as such. By treating regressions as learning opportunities
you gain knowledge of your system and the systems it interacts with, i.e., you
become a better software engineer. This chapter provides a sequence of questions
and reminders to help in a scientific approach to performance regression
debugging. We hope it aids you well.

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
this step is to observe how the problem *presents* itself in the system. No
phenomena exists without leaving a trail of evidence. Our purpose in this step
is to find this trail and re-state the problem description *in terms* of the
system. You should ask yourself the following questions:

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

With these questions in mind, the next step is to gather data [#]_ . You want to
gather enough data to be able to precisely describe the problem at hand. "The
system slows to a crawl" is not a good description of the failure mode because
it is too vague. How does the system slow? At what point does it slow? On what
input does it slow? Does this always happen or is it a random occurrence? "The
system consumes 36Gb out of 32Gb of memory on input ``foo``, which forces the
operating system to begin paging" is a good description of the failure mode. It
is a precise description; we know that whatever the root cause is it affects the
runtime memory cost of our program. Similarly, this description gives an
estimate of the magnitude of the problem.

This step is concluded when you have the following information:

#. You can precisely describe the problem *in terms* of the sub-systems of your
   program. These sub-systems could be your own components or they could be
   sub-systems your program relies upon. For example, you should be able to
   precisely describe the increase in CPU cycles, runtime memory cost, missed
   branches, cache writes/reads misses, network requests, dropped packets etc.
   the failure mode incurs compared to the baseline (i.e., compared to normal
   operation).

Create the smallest reproducible test of the problem
----------------------------------------------------

Once you have characterized the problem you should try to create an isolated,
minimal test to reproduce the problem. Try to construct the test such that it
interacts with as few sub-systems and external systems as possible to limit the
scope of the investigation. At the end of the investigation, you can add this
test to your testsuite to ensure the problem does not manifest again.

Creating the a reproducible test is never the easy part, but it is not
impossible. To construct the test case try the following steps:

#. Try to isolate the sub-systems and external systems that you suspect are
   likely to be in the failure mode.

#. Each external system provides information or a service to your system. Try to
   reproduce these dependencies in a deterministic way and treat them as inputs
   to your test case.

#. Try to isolate the code you believe to be in the failure mode. This should
   follow almost directly from characterizing the problem. Tools such as
   valgrind, which provide line by line information of source code, are helpful
   here.

#. Remove all domain-specific information. Think of the possible failure mode
   from the perspective of the system. Do not think in terms of your business
   logic; using concepts such as ``Customer``, ``Bank Account``, or ``Payment
   Information``. Instead, think in terms of the realization of these concepts
   in your system. ``Customer`` is a ``String``, ``Bank Account`` is a
   ``Integer``, ``Payment information`` is a ``String``. Now re-describe the
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
code directly or indirectly uses, is a character. Each function you have written
is a character; and so on. These are the objects of your hypothesis; they are
what the hypothesis makes a statement about, and define the sequence of
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


Predict the Response
--------------------

Once you have a hypothesis

Summary
-------


.. [#] Be sure to have a reproducible testing environment setup before you begin
       gathering data. :ref:`Repeatable Measurements`
