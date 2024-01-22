.. _How to Debug chapter:

How To Debug
============

This chapter presents a recipe for debugging performance regressions in Haskell.
Often, when we debug code, it becomes too easy to begin shotgun debugging; we
apply a bunch of best-guess changes and retest to see if our stimulus induced a
response. You should do your best to avoid these urges. Instead, a more
effective method is to use a scientific approach, and develop a hypothesis and
conceptual model of how the bug manifested. Every bug or performance regression
is a learning opportunity and should be considered as such. By treating
regressions as learning opportunities, you gain knowledge of your system, the
quality of its design, and how the system interacts with its environment. But
more importantly you become a better software engineer. This chapter provides a
guide to aid you in debugging performance regression. We base it off of David
Agan's `9 Rules for Debugging <https://debuggingrules.com/>`_ book [#]_ and
apply his insights to Haskell programs.

Vocabulary
----------

Unless otherwise noted, we use the following vocabulary to describe aspects of
our optimization journey. Because these do not have a formal definition, we
present them here instead of in the :ref:`glossary`:

1. *The system*: The system is the entire computational edifice that you're
   constructing. This includes your operating system, your CPU, your memory
   controller and the program that you have written.

2. *The program*: The program is the program we are trying to optimize that runs
   on the system.

3. *The problem* or *the bug*: The problem is an observable phenomenon of the
   program. It is the performance regression we are trying to characterize,
   understand, fix and prevent.

4. *The failure mode*: The failure mode is the sequence of interactions between
   sub-systems or external systems and your system that manifest the problem.

5. *The baseline*: The baseline is the observable, measurable behavior of the
   program which constitutes *normal and acceptable operation*. This what you
   compare with to know you have a problem.

The Goal
--------

We have two goals when debugging. First, we wish to repair [#]_ our system as
fast as possible. Second, and more importantly, we wish to gain a deeper
understanding of our system. The value communicated by the second goal far
outweighs the first. By taking the opportunity to gain a deeper understanding,
we are empowering ourselves *and* everyone who reads our documentation to write
*more effective* code in the future. Ideally this translates to preventing
regressions before they can manifest, which will always cost less engineering
time and resources than reacting to regressions after they occur.

The best way to achieve these goals is, paradoxically, to work slowly,
contemplatively and deliberately. By working deliberately, you can ensure that
your debugging is making progress and can rigorously test your mental model of
the system. Changes to your code should verify that your mental model is either
correct or incorrect.

Agan's Rules of Debugging
-------------------------

There are nine rules, ordered from most important to least. We'll take them one
by one and relate each to Haskell. Note that we do not repeat *all* of Agan's
advice, only the central ideas and how they relate to Haskell. This way all
readers will still benefit from Agan's work, which we strongly encourage.

.. _Understand the System:

Understand the System
^^^^^^^^^^^^^^^^^^^^^

To debug, optimize, iterate and improve, you must understand the current system,
otherwise how will you know if the system is operating correctly and therefore
if your performance is real [#]_ . Furthermore,
not knowing the system will limit the optimizations that you will be able to
conceive and implement.

Understanding the system is challenging for a high-level language such as
Haskell and is often a barrier to optimizing and debugging performance of
Haskell code. For example, GHC might surprise you by not inlining a particular
function and therefore a cascade of optimizations have not taken place.

Fear not, this is why this book exists. For our purposes, learning the system
means understanding your program, how GHC compiles your program, and likely some
aspects of GHC itself. Note that this is not unique to Haskell. If one were
trying to optimize a C program they would need to understand the program, its
interaction with whichever C compiler, and perhaps the interaction with the
operating system or even the CPU caches depending on the optimizations they
desire to implement and their performance goals.

This book cannot help you learn your own program, but it can help you learn GHC.
To understand the system, begin with learning to read the intermediate
representations of your program such as :ref:`Core <Reading Core>`, or :ref:`Stg
<Reading Stg>`. Reading the intermediate representations are prerequisites for
understanding the :ref:`GHC Optimizations <GHC Optimizations>` that GHC performs
which make Haskell so fast; and so slow when they do not fire.

.. _Make it Fail:

Make it Fail
^^^^^^^^^^^^

To make it fail, means to identify and have control over a stimulus that
*induces* incorrect system behavior. This is the phase where one is searching
for a small program, called a :term:`reproducer`, that induces the malformed
behavior. A reproducer is imperative because it gives control and a litmus test.
With a reproducer one can observe the problem *at will* and repeatedly test the
bug to determine when it is fixed in preparation for a repair.

In Haskell, the search for a reproducer is no different than in any other
language. Try to start from a known state [#]_; use the exact hardware and software
if possible. The closer the hardware and software matches, the less variables
you have to consider. Then, try to automate as much as possible with a script. A
script pays dividends in the long run. It details the exact sequence of events
that produces the bug and leaves out any guess work. You should expect to run
the reproducer numerous times over weeks and months in the worst case and a
script will help you keep things tidy and controlled.


.. _Don't Think, Look:

Don't Think, Look
^^^^^^^^^^^^^^^^^

Of all of Agan's rules, this is the rule that Haskellers struggle to do the most
often. We enjoy thinking with types and abstractions rather than instrumentation
and measurement. To don't think, look means to check the instrumentation or add
instrumentation to check, and then use the instrumentation to confirm or reject
your hypothesis. Measurement, and observation will be faster on average than
:term:`shotgun debugging` unless you are very lucky (which of course is not
reliable).

So what is our instrumentation? We can directly observe by reading the
intermediate representations such as :ref:`Core <Reading Core>`, :ref:`Stg
<Reading Stg>`, or :ref:`Cmm <Reading Cmm>`. Reading the intermediate
representations works well if you suspect an optimization is not firing; which
can often happen during upgrades of GHC or dependencies. Or we can use a probe
to inspect the system. The available probes range across the entire Haskell
software stack, from binary probes, such as :ref:`perf <Perf Chapter>` and
:ref:`valgrind <Valgrind Chapter>`, to GHC provided probes such as
:ref:`eventlog <Eventlog Chapter>` and :ref:`GHC-debug <GHC Debug chapter>`.
Note that there is no best probe, rather the right probe will depend on the bug
and your exact situation. For example, using eventlog to inspect your program's
heap is typically the first check of instrumentation if you suspect a memory
leak.

.. note::

   See the :ref:`Measurement, Profiling, and Observation <MPO>` for a complete
   list of instrumentation.

Once you know which instrumentation to use and how to interpret its output,
search its output until you identify a handful of causes and have at least one
failure mode hypothesis to test. Remember that you are not observing *the bug*
with the instrumentation, rather you are observing *the effect* of the bug in
order to formulate a hypothesis. For example, with a memory leak, the bug's
effect could be a high amount of memory usage reported by GHC, or your operating
system, or a pyramid shaped heap profile. For a missed optimization, the effect
could be redundant boxing or a missed rule that produces poor performing Core,
and consequently a higher Mutator time reported by the RTS.


.. _Divide and conquer:

Divide and Conquer
^^^^^^^^^^^^^^^^^^

..
   what is divide and conquer

Imagine the system execution as an ordered linear sequence of causal events
:math:`e_{0} \rightarrow \ldots{} \rightarrow e_{halt}`, where :math:`e_{0}` is
the first event to take place and :math:`e_{halt}` the last. When the system has
a bug, the sequence of events, also called a causal chain, diverges from its
expected behavior at some event, :math:`e_{bug}`. In this view, debugging is
searching the causal chain for :math:`e_{bug}`. Thus, to divide and conquer
means to search the causal chain with the `divide and conquer strategy
<https://en.wikipedia.org/wiki/Divide-and-conquer_algorithm>`__.

To divide and conquer the causal chain, start with the anomalous end. Think of
:math:`e_{bug}` as a pivot point, after the bug the chain is: :math:`e_{bug}
\rightarrow \ldots{} \rightarrow e_{halt}` and the system is in an anomalous
operating state, before the bug: :math:`e_{0} \rightarrow \ldots{} \rightarrow
e_{bug-1}` the system is in an acceptable operating state. So if we start from
:math:`e_{0}` then we must verify the system state at every event :math:`e_{0}
\ldots e_{bug-1}` *on all possible* control flow branches. That is a lot of work
(and is better left to `assertions
<https://en.wikipedia.org/wiki/Assertion_(software_development)>`__ ). However,
by beginning the search on the anomalous side we only have to find *one*
anomalous state on *one* branch to begin to work backwards to :math:`e_{bug}`.
Thus, searching from the anomalous end is faster because there are less possible
system states to check.

..
   how to divide and conquer in haskell

A good tactic to make the search for :math:`e_{bug}` easier is to exacerbate the
effect of the bug, or in the word's of David Agan: "Make it obvious.". Our
Haskell programs, like all programs, obey this causal chain. But Haskell is a
pure lazy language, so the causal chain forms via data dependency rather than
forming via the observable ordering of side-effects [#]_. This simplifies
debugging because we control the data and therefore the causal chain. So by
changing the input to the system, we can make the rough location of
:math:`e_{bug}` more obvious. Making the bug's effect obvious can be as simple
as making the load on the system larger. For example, imagine trying to optimize
the Fibonacci function, instead of testing with ``fib 10`` one can use ``fib
200`` to exacerbate memory or runtime issues. This will create a larger response
signal in the instrumentation which is easier to find, diagnose and analyze.

A similar method is to input data that has an easily recognizable pattern. This
technique is useful when reading Core, Stg, or Cmm. GHC generates names based on
user provided names and keeps the original names in the intermediate
representations. For example, consider this code

.. exec::
   :context: true
   :process: haskell
   :intertext: and its Core output (the 0 at the end is the program's result):

   {-# OPTIONS_GHC -dsuppress-all -ddump-simpl  #-} -- dump the Core

   module Main where

   main :: IO ()
   main = print $ f 0 True
     where
       f x y = let j :: Int -> Int
                   j 0 = 0
                   j n = j (n-1)
               in case y of
                 True -> j 22
                 False -> j 33

Notice that the user names ``f`` and ``j`` are still in the Core output as
``f_r17p`` and ``j_azm``, but both have been transformed into an
:term:`Occurrence Name`. So we can use more obvious names to make searching the
intermediate representations faster. For example, instead of ``f`` and ``j`` we
can use the obnoxious ``fFINDME`` or ``jLOOKDONTTHINK``:

.. exec::
   :context: true
   :process: haskell

   {-# OPTIONS_GHC -dsuppress-all -ddump-simpl  #-} -- dump the Core

   module Main where

   main :: IO ()
   main = print $ fFINDME 0 True
     where
       fFINDME x y = let jLOOKDONTTHINK :: Int -> Int
                         jLOOKDONTTHINK 0 = 0
                         jLOOKDONTTHINK n = jLOOKDONTTHINK (n-1)
                     in case y of
                       True -> jLOOKDONTTHINK 22
                       False -> jLOOKDONTTHINK 33

And now it is much easier to recognize or search for these names.

.. _Change one thing at a time:

Change One Thing at a Time
^^^^^^^^^^^^^^^^^^^^^^^^^^

To some extent, everyone understands that changing only one thing at a time is
good practice. It simplifies keeping a log of changes, correlating cause and
effect, and more importantly, it reduces the probability of creating an abnormal
system. Every change to the system has a chance to move that system's operation
from inside the system's `engineering tolerance
<https://en.wikipedia.org/wiki/Engineering_tolerance>`__ (the expected range of
operation) to outside; into new and unexplored operating conditions. By changing
only one thing at a time we mitigate this risk while debugging.

Doing this on a Haskell code base will be identical to any other programming
environment; Haskell is not unique here. But we have two recommendations: first,
make sure you have a baseline, a working master copy, so that you can always
compare your working copy to it. Second, if you begin changing lots of parts of
the system semi-randomly (such as adding a bunch of strictness) to check if
these changes affect the bug's effect, then you are guessing, and instead should
:ref:`Don't Think, Look`.

.. _Keep an Audit Trail:

Keep an Audit Trail
^^^^^^^^^^^^^^^^^^^

To keep an audit trail means to maintain a log of your debugging work. Think
like a scientist who wants others to be able to replicate their work. Be
meticulous, you should write down the exact sequence of what you did, and then
what and how much happened. You should write your log as if you would return to
it years or months later. You should record your theories. Theories are the
background context that informed your change; *the reason*, *the why* that you
made the change you did. In addition, record the commands you ran, how you first
observed the bug's effect, the instrumentation you used to monitor the bug's
effect and what you expected to observe from a change. A good log should tell a
story; it should read like the laboratory journal of a scientist or engineer.
Lastly, if the effect of the bug is some special piece of output, then be sure
to include it so that you create a searchable document, you're future self will
be thankful.

An example log might look something like this:

.. note::
   In the example, I use ``foo`` and ``bar``  as meta-variables that stand for a
   subsystem or test. Similarly, I use angle-bracket notation ``<...>`` to
   represent pieces of important data that the log should record.


.. code-block:: text

   * Mutator regression in commit <some-hash> | System version <i> | Feb. 02, 2024

     ** Bug's Effect:

        - We've observed in increase of <n> seconds (<m> %) in Mutator time as
        reported by the RTS's -S flag when running test foo on debian 10 at
        commit <hash> in CI.

     ** Background:

        Test foo is memory intensive, testing a pathological case where user
        input results is subsystem bar performing a lot of IO operations
        concurrently.

     ** Sanity Checks:

        - Do we observe this regression on other platforms, e.g., Windows,
          Fedora or Mac?

        - Has the CI runner changed?

     ** Theory: Regression caused by missed optimizations resulting in a GHC
     version bump that occurred at commit <hash>.

     *** Possible Instrumentation

        - Compare Mutator time with the baseline using -O0. I expect to see the
          baseline and regressed versions perform similarly at -O0. If that is
          the case then we should disable the optimizations implied by -O1 one
          by one. Why -O1? Because that is -O2 with two less passes of GHC's
          optimizer. If that is not the case, then we should check versioning
          differences and bisect the commits to find the commit where the
          mutator regresses. In this scenario it is likely that we've slowed the
          system rather than some interaction with GHC. We could also check for
          a stack leak in this case.

        - Revert to known working GHC version, then run test foo on debian 10 at
          commit we observed the bug. If we observe the bug's effect then we
          know its a regression that in our code base. If not then its a
          regression in a dependency and we can systematically test each one.

        - Inspect the tickyticky output to compare the baseline and regressed
          branch. Check for a change in the number of unboxed tuples and data
          constructors. Unsure how stable these numbers are between runs of the
          same test.

        - Compare the output of the baseline branch and the regressed branch
          when compiling with `-Wall-missed-specializations`. Changes in this
          output could point to missed specializations which would also be
          observable in Core. However, this output also changes along dependency
          versioning. I expect to see minor differences in the output. If these
          differences occur in the code the foo exercises then they are likely
          candidates for the regression. Will have to verify by reading Core.

     *** Tests

        - Compare Mutator time with baseline version using -O0:
            Note taken on [2021-12-03 Fri 15:55]

            With `ghc --version`: 9.8.1

            Ran `cabal
            test --show-details=streaming --pattern='foo' --ghc-options='+RTS -S
            -RTS'` on both baseline (commit <hash>) and regressed (commit
            <hash>). Results differed by less than 1%.


     ** Theory: Regression caused by regression in a dependency
     ...

.. _Check the Plug:

Check the Plug
^^^^^^^^^^^^^^

To check the plug means to verify your assumptions by performing sanity checks.
Assumptions could be library and GHC versions, your operating systems' available
resources and settings; such as the CPU governor (be sure to make sure your
laptop is plugged in). But assumptions can also be environmental, for example,
the input data is what you expect, or that you are working from the commit you
expect to be working from.

Checking the plug enables one to divide and conquer. Without checking the plug,
the anomalous region of the system's causal chain is the whole chain! So make
sure you take the time to check the plug. To check the plug, start at the
beginning of the causal chain; check the tools, the dependencies, the inputs and
default settings; as David Agan states: "Many anomalous systems are created by
default settings."

Ideally you would check the plug at the beginning of your investigation so that
your investigation does not proceed with bad assumptions. Another time to check
the plug is when your investigation has led you into contradiction. This happens
when you follow two hypotheses that are both confirmed with testing, but which
contradict each other. This is typically an indication that something is
egregiously wrong, and that the system you are inspecting is far outside its
engineering tolerances. Such deviations are more times than not (but not
always!) caused by unplugged things in the system.

.. _Get a Fresh View:

Get a Fresh View
^^^^^^^^^^^^^^^^

Sometimes, despite your best efforts you'll exhaust all leads and become stuck.
To get a fresh view, means to recognize when you're stuck and ask for help,
especially from experts.

The value in asking for help is a new perspective on the problem. The new
perspective may highlight features of the problem that you missed or overlooked,
and often times describing and explaining the issue to another person can lead
you to new insights.

When asking for help, do not communicate your theories, instead communicate the
symptoms you've observed. If you communicate your theories then you'll
inevitably lead your interlocutor down the path your investigation took; you'll
accidentally coerce them to your perspective. So resist the temptation, allow
them to come to their own conclusions and formulate their own theories.

Fortunately, the Haskell ecosystem is full of enthusiastic, helpful people from
all over the world. Don't be afraid to participate! The worst case outcome is no
one responds, the best case is that you connect with others who share your
passion and help you fix the problem. Here are the best forums to reach out:

* `The Haskell discourse <https://discourse.haskell.org/>`__
* `The mailing lists <https://wiki.haskell.org/Mailing_lists>`__ (see `here <https://mail.haskell.org/mailman/listinfo>`__
  for a comprehensive list.)
* If you believe its GHC related then the GHC Developers' :ghcWiki:`mailing list
  <mailing-lists-and-irc>` is the place to ask.
* `The Haskell subreddit <https://www.reddit.com/r/haskell/>`__.


.. _If You Didn't Fix It, It Ain't Fixed:

If You Didn't Fix It, Then It Ain't Fixed
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Repeat it with me: "The problem will not fix itself nor will the system correct
itself." We wish it were otherwise, but the cost of inaction almost always
outweighs the cost of action because inaction prevents quality control.

So once you have a fix in mind how do you know it will correct the system? You
check. You must :ref:`Don't Think, Look`! To verify that the fix corrects the
system, toggle the fix on and off, and observe if the toggle also toggles the
bug's effect. If the bug's effect also toggles then you should have high
confidence that you've found a fix because you can now affect the bug at will,
and therefore you have regained control of the system.

Control of the system is crucial; if you do not regain control, then you cannot
be sure the bug will not manifest again and have lost some understanding of what
the system is. In other words, it is only with control of the system that you
are able to make correctness and performance guarantees to your end-users.
Furthermore, it is only with control that you can begin to craft a repair,
complete with instrumentation to capture more system details for next time.


Summary
-------

We've come a long way, let's review, the nine rules of debugging are:

1. :ref:`Understand the System`: If you do not understand the system then you
   cannot debug it.

2. :ref:`Make it Fail`: Find a reproducer. This is the beginning of regaining
   control of the system.

3. :ref:`Don't Think, Look`: Check your instrumentation, if you do not have
   instrumentation then add it. Observation is reliably faster than guess work.

4. :ref:`Divide and conquer`: Search for the bug by dividing and conquering.
   start from the anomalous end of the system, verify its anomalous, then go to
   sound end of the system, verify its sound, now repeat until you close in on
   the bug.

5. :ref:`Change one thing at a time`: Don't shotgun debug, change only one thing
   at a time to mitigate the risk of moving the system so far outside its
   engineering tolerances that the bug's effect is obscured by newly induced
   bugs.

6. :ref:`Keep an Audit Trail`: You are a scientist. Keep a laboratory journal of
   your work so that you can reproduce it months or years later. Make it
   searchable, make it precise, give it background context and record your
   theories.

7. :ref:`Check the Plug`: Verify your assumptions before diving into a rabbit
   hole and when you conclude in a contradiction. And always check the default
   settings!

8. :ref:`Get a Fresh View`: Ask for help, especially from an expert. Only report
   your observations and data, don't report your theories.

9. :ref:`If You Didn't Fix It, It Ain't Fixed`: The system will not repair
   itself, and even if it did you would not regain control of the product that
   you ship. Take the time and put in the work to find a repair, this will
   always be faster in the long run than ignoring the bug and relying on hope.

Why do we follow these rules? Because doing so is more efficient than shotgun
debugging, guessing, or living with a buggy system. Recall our goals: in the
short term, to repair the system; and in the long term, to gain a deeper
understanding of the system. We are thinking on a time scale of years. On that
time scale bugs are inevitable. By gaining a deeper understanding of the system
we slowly master the system. Mastering the system, in turn, enables more
efficient, better engineered systems, more communicative documentation, and the
ability to avoid future bugs *before* they manifest. So work slowly,
deliberately and carefully. The investment pays off in the long run.


.. [#] We cannot recommend this book highly enough, it should be mandatory
       reading for all software engineers.

.. [#] A *fix* is the act of bringing a system back into an acceptable operating
       state. A *repair* is a fix with an understanding of the mechanism and
       chain of events (failure mode), that moved the system outside of its
       engineering tolerances and into an unacceptable operating state. We seek
       to repair, not simply to fix.

.. [#] In `episode 21
       <https://www.typetheoryforall.com/2022/08/04/21-Conal-Eliott-2.html>`__
       (at 45 minutes) of the `Type Theory For All
       <https://typetheoryforall.com/>`__ podcast, `Conal Elliot
       <http://conal.net/>`__ persuasively argues that proof of correctness is
       necessary for efficiency and system performance. The idea is that proof
       of correctness protects us from being fooled that our optimized
       implementation is correct rather than *appearing* to be correct, and
       therefore that we are optimizing the system that we originally intended
       to build and not a slightly adjacent, incorrect, but fast one. Or in
       other words, performance without correctness is easy, but easily useless.

.. [#] Be sure to have a reproducible testing environment set up before you begin
       gathering data. See :ref:`Repeatable Measurements`.

.. [#] `Alexis King <https://lexi-lambda.github.io/>`__ has a great `video
       <https://youtu.be/fSqE-HSh_NU?si=GE1RMlY_6OIumuri>`__ on the impact of
       strictness on a programming language's semantics from the perspective of
       a compiler. The essential idea is that a program is a specification of
       behavior, which from the purview of a compiler is a set of constraints
       that dictate the observable behavior of the program. From this
       perspective, a strict evaluation strategy introduces synthetic
       dependencies between every statement in a program which then dictates the
       evaluation order. In contrast, a lazy evaluation strategy imposes no such
       synthetic dependencies, thereby allowing data dependencies in the program
       to dictate the evaluation order and allowing the compiler to perform more
       optimizations.
