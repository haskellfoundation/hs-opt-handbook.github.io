*****************************
Haskell Optimization Handbook
*****************************


Welcome! This repository contains the source of `Haskell Optimization Handbook
<https://input-output-hk.github.io/hs-opt-handbook.github.io/>`_


What is the Point?
==================

In not so many words:

1. Haskell is a great and fast language
2. Understanding the performance of Haskell programs is hard, because:

   1. Not many developers have direct experience with lazy evaluation.
   2. Resources for understanding performance are distributed around the web in
      wikis, blog posts etc.
   3. Performance optimization in Haskell requires a lot of up front knowledge,
      including profiling techniques, the memory model or understanding GHC's
      intermediate representations

The Goal
========

Our mission is to create a *single resource* that makes available information
previously reserved for the highest level of arcane functional programming
wizards. We envision a handbook that any Haskeller can turn to, to gain enough
actionable knowledge to begin performance optimization on their programs; with
topics ranging from profiling/debugging and choosing appropriate libraries to
low level optimization techniques such as exploiting ``Levity polymorphism`` to
control memory representation and layout.


Contributing
============

Yes! We need your help! There are many ways to contribute and each have their
own contributing guide. Please start by reading through the `Working Conventions
<https://github.com/input-output-hk/hs-opt-handbook.github.io/tree/main/Contributing>`_
and `Style Guide
<https://github.com/input-output-hk/hs-opt-handbook.github.io/blob/main/style-guide.rst>`_
to familiarize yourself with the project expectations and workflow. We make
extensive use of checklists to track and manage state, so If you:

- Want to suggest a new section, please open an issue and we'll triage in the issue.
- Want to be an editor, please read the `Editor Contribution guide
  <https://github.com/input-output-hk/hs-opt-handbook.github.io/blob/main/Contributing/Editing.rst>`_.
- Want to be a writer. We split the writing by book topic, so:

  - Write a Case Study; see the `Case Study Contribution Guide
    <https://github.com/input-output-hk/hs-opt-handbook.github.io/blob/main/Contributing/CaseStudies.rst>`_.
  - Write a profiling or debugging section; see the `Part 1: Measurement and Observation Contribution Guide
    <https://github.com/input-output-hk/hs-opt-handbook.github.io/blob/main/Contributing/Measurement_Observation.rst>`_.
  - Write an optimization section; see the `Part 2: Optimizations Contribution Guide
    <https://github.com/input-output-hk/hs-opt-handbook.github.io/blob/main/Contributing/Optimizations.rst>`_.

Lastly, you might also benefit from reading the original `Haskell Foundation
proposal
<https://github.com/doyougnu/tech-proposals/blob/hs-opt-handbook/proposals/2022-01-31-haskell-optimization-handbook.md>`_

What's in it for me?
--------------------

How did you learn Haskell? Did you pick it up from Learn You a Haskell or one of
the many other text books? This is your chance to give back to others who are
beginning their Haskell Journey and to make the Haskell community a little bit
better. Even if you do think you are not confident in your Haskell skills just
reading and marking sections of text that are confusing would be a big help!
Haskell has a reputation for being beginner unfriendly, and for a long learning
curve, but this does not have to be the case. As a community we can, and should,
do better, and you can begin doing that right now with this project.


Building
========

Requirements
------------

The project is built with ``sphinx`` and written in ``RestructuredText``. Make
sure the following is installed and available on ``$PATH``:

- ``python 3.9``
- ``sphinx 4.5.0``

If you're using ``nix`` or ``NixOs`` then you'll find the appropriate ``.nix``
files in the root directory of the project.

Invocations
-----------
Non-nix, run:

.. code:: shell

   make html

With nix and flakes, run:

.. code-block:: bash

   nix develop -c make html

With nix and flakes, and to autobuild run:

.. code-block:: bash

   nix develop -c sphinx-autobuild . _build/html

With nix and no flakes, run:

.. code-block:: bash

   nix-shell --run 'make html'

To rebuild the book everytime any ``*.rst*`` file changes do:

.. code-block:: bash

   find . -name "*.rst" | entr -sc '<your-build-command-here>'

or use ``sphinx-autobuild``:

.. code-block:: bash

   sphinx-autobuild . _build/html


You can then check the output in ``_build/html`` or load directory into whatever
browser you'd like:

.. code-block:: bash

   firefox _build/html/index.html
