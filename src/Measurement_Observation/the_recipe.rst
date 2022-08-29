
The Recipe
==========

This chapter presents a recipe for debugging performance regressions in Haskell.
This is a philosophical, not tool based chapter. Often times when we need to
debug code it becomes too easy to trace execution or use a shotgun approach;
where we apply a bunch of changes and retest to see if our stimulus presents a
response. You should do your best to avoid these urges. Instead, use a
scientific approach and develop a hypothesis and conceptual model. Every bug or
performance regression is a learning opportunity and should be considered as
such. By treating regressions as learning opportunities you gain knowledge of
your system and the systems it interacts with, i.e., you become a better
software engineer. This chapter provides a sequence of questions and reminders
to aid in a scientific approach to performance regression debugging. We hope it
aids you well.

0: Characterize the problem
---------------------------

The first step to solving any kind of problem is characterization. We want to
gain knowledge about the *nature* of the problem at hand, and how that problem
manifests in our system. You should ask yourself the following questions:

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

1: Define a Hypothesis
----------------------

The objects of the hypothesis
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Think of characters in a story book. These are the objects of your hypothesis;
any system that takes an action to produce a result that your code interacts
with or causes, is a character. Each data structure your code directly or
indirectly uses, is a character. Each function you have written is a character.
And so on.

Defining a good hypothesis
^^^^^^^^^^^^^^^^^^^^^^^^^^

Not all hypotheses are created equal. Good hypotheses have the following
properties:

#. A good hypothesis yields information when confirmed *and* when invalidated. A
   bad hypothesis *keeps constant* the level of information you have about the
   phenomena. In other words, a bad hypothesis is one where you only gain
   information if the hypothesis is shown to true.

#. Specificity: Good hypotheses are specific enough *to be* invalidated. Here
   are some examples:
   #.





2: Create the smallest possible test of the Hypothesis
------------------------------------------------------

3: Predict the Response
-----------------------
