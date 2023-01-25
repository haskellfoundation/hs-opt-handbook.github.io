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
      intermediate representations.

The Goal
========

Our mission is to create a *single resource* that makes available information
previously reserved for the highest level of arcane functional programming
wizards. We envision a handbook that any Haskeller can turn to, to gain enough
actionable knowledge to begin performance optimization on their programs; with
topics ranging from profiling/debugging and choosing appropriate libraries to
low level optimization techniques such as exploiting ``Levity polymorphism`` to
control memory representation and layout.

Sponsors
========

This work has been generously sponsored by `IOG <https://iohk.io/>`_. We thank
them for their contribution and enrichment of the Haskell ecosystem.

Contributing
============

Yes! We need your help! It does not matter if you're a new Haskeller, a grey
beard, or an arcane functional wizard, there is a way for you to contribute!
Please see the `Contributing guide
<https://github.com/input-output-hk/hs-opt-handbook.github.io/blob/main/Contributing.rst>`_

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
