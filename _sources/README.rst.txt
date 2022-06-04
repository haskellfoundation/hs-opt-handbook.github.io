=============================
Haskell Optimization Handbook
=============================

Welcome! This repository contains the source of `Haskell Optimization Handbook
<https://input-output-hk.github.io/hs-opt-handbook.github.io/>`_


What we are trying to fix
-------------------------

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
-----------

Our mission is to create a *single resource* that makes available information
previously reserved for the highest level of arcane functional programming
wizards. We envision a handbook that any Haskeller can turn to, to gain enough
actionable knowledge to begin performance optimization on their programs; with
topics ranging from profiling/debugging and choosing appropriate libraries to
low level optimization techniques such as exploiting ``Levity polymorphism`` to
control memory representation and layout.


Contributing
------------

Yes! We need your help! There are many ways to contribute and each have their
own `contributing guide <../Contributing/>`_. Please make sure to read through
the `Working Conventions <../Contributing/README.rst>`_ for the project so that
your work is more easily merged. If you:

- Want to suggest a new section, please open an issue and we'll triage in the issue.
- Want to be an editor, please read the `Editor Contribution guide
  <../Contributing/Editing.rst>`_.
- Want to be a writer. We split the writing by book topic, so:
  - Write a Case Study; see the `Case Study Contribution Guide <../Contributing/CaseStudies.rst>`_.
  - Write a profiling or debugging section; see the `Part 1 Contribution Guide <../Contributing/Part1.rst>`_.
  - Write an optimization section; see the `Part 2 Contribution Guide <../Contributing/Part2.rst>`_.

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

Building
--------
Non-nix, run:
.. code:: shell
          make html

With nix and flakes, run:
.. code-block:: bash
   nix develop -c make html

With nix and no flakes, run:
.. code-block:: bash
   nix-shell --run 'make html'

To rebuild the book everytime any ``*.rst*`` file changes do:
.. code-block:: bash
   find . -name "*.rst" | entr -sc '<your-build-command-here>'
