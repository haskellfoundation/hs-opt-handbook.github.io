
The Recipe
==========

This chapter presents a recipe for debugging performance regressions in Haskell.
This is a philosophical, not tool based chapter. Often times when we need to
debug code it becomes too easy to trace execution or use a shotgun approach;
where we apply a bunch of changes and retest to see if our stimulus presents a
response. You should do your best to avoid these urges. Instead use a scientific
approach and develop a hypothesis and conceptual model. Every bug or performance
regression is a learning opportunity and should be considered as such. By
treating regressions as learning opportunities you gain knowledge of your system
and the systems it interacts with, i.e., you become a better software engineer.
This chapter provides a sequence of questions and reminders to aid in a
scientific approach to performance regression debugging. We hope it aids you
well.

0: Characterize the problem
---------------------------

The first step to solving any kind of problem is characterization. We want to
gain knowledge about the *nature* of the problem at hand, and how that problem
manifests in our system. You should ask yourself the following questions:

#. Have I observed this problem before? Is it a known failure mode? If so then
   why does it continue to happen?

#. Is the problem deterministic? Or is it stochastic? If it is stochastic what
   is the rate at which we observe the problem phenomena?

#. Is there anything *unique* about the environment the problem manifest in?
   Specifically:
   -. one
   -. two
   -. three

1: Define a Hypothesis
----------------------

It is

2: Create the smallest possible test of the Hypothesis
------------------------------------------------------

3: Predict the Response
-----------------------
