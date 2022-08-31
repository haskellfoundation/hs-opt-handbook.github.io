
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
   environment the system operates in that could induce the problem?

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


Define a hypothesis
-------------------

The objects of the hypothesis
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Think of characters in a story book. These are the objects of your hypothesis;
any system that takes an action to produce a result that your code interacts
with or causes, is a character. Each data structure your code directly or
indirectly uses, is a character. Each function you have written is a character.
And so on.

Defining a good hypothesis
^^^^^^^^^^^^^^^^^^^^^^^^^^

..
   gain knowledge about the *nature* of the problem at hand, and
   the *failure mode*, i.e., how that problem manifests in our system.


Not all hypotheses are equal. Good hypotheses have the following
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


Create the smallest possible test of the Hypothesis
---------------------------------------------------
Once you have

Predict the Response
--------------------

Summary
-------


.. [#] Be sure to have a reproducible testing environment setup before you begin
       gathering data. :ref:`Repeatable Measurements`
