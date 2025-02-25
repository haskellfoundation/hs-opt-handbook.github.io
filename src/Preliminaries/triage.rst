.. _Triage:

========
 Triage
========

This is a triage; it is the signpost that marks the start of your journey and
should give you enough direction to make your next steps.

Symptoms
--------

You do not have a problem, but want to learn performance-oriented Haskell
    Begin with the :ref:`Philosophies of Optimization <Philosophies of
    Optimization Chapter>`. Then read the :ref:`the programs of consistent
    lethargy <What Makes Fast HS Chapter>`, and some of the case studies.  This
    should give you enough to decide your next steps. If you decide to begin
    doing some optimizations see the :ref:`checklist <The Checklist>` for more
    ideas.

You have a performance regression that you want to understand and fix
    You need to diagnose the regression and begin thinking in terms of an
    investigation. Read :ref:`how to debug <How to Debug Chapter>` to make sure
    you know how to make progress. Since you have observed a regression, try to
    find a commit or state in your project where you *do not* observe the
    regression. This will let you bisect your project to narrow down the space
    of changes that START. You may also consider other forms of profiling and
    observation, such as:

    - Running a :ref:`tickyticky <Ticky Chapter>` profile.
    - :ref:`Checking the heap <Eventlog Chapter>`.
    - Inspecting the :ref:`Core <Reading Core>`.
    - Inspecting the :ref:`STG <Reading STG>`.
    - Observing the :ref:`cache behavior <Cachegrind Chapter>`.
    - Observing the :ref:`CPU's Performance Counters <Perf Chapter>`.

You have a program that you want to begin optimizing
   If you are short on time, begin with the :ref:`checklist <The Checklist>`
   and then check for :ref:`memory leaks <Eventlog Chapter>`. If not, begin with
   the easy changes:

   - Use better datastructures.
   - Carry checks in the type system so that the program is not always checking
     the same predicates.
   - Filter before you enter a hot loop.
   - Remove niceties in the hot loops, such as logging.
   - :ref:`Check the heap <Eventlog Chapter>`. The :ref:`klister <Klister Case
     Study>` case study is a good example of this kind of optimization.

   Then move into the more invasive changes such as:

   - :ref:`unrolling <Unroll Monad Transformers Chapter>` your monad transformers.
   - Using the :ref:`one-shot monad trick <OneShot Monad Chapter>`.
   - Selectively :ref:`defunctionalizing <Defunctionalization Chapter>` critical functions.
   - Critically analyzing your architecture from a performance perspective.

You have a program that you've optimized, but want to optimize more
    If you have already harvested the low hanging fruit then you have likely
    driven the program into a local maxima. Therefore, if you still need more
    speed then you must make more invasive changes, such as we listed
    above. However, the best changes you can make will exploit properties of the
    problem domain to reduce the work your program must do to arrive at a
    result. Often times these will be architectural changes.

    .. todo::

       In lieu of having links for you continue in this case. You can search for
       data-oriented design to begin refactoring your system in this manner. I
       highly recommend this `this talk
       <https://youtu.be/IroPQ150F6c?si=mD486UkpWquFygjr>`__ by Andrew Kelley.
