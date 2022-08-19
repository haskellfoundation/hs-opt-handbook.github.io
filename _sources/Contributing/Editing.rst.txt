:orphan:

=================
How to Contribute
=================


As an Editor
------------

Introduction
============

This is a checklist for editors of (Optimizations) of the Handbook. A word to
editors. Writing is hard and it is easy to get frustrated. Good writing is even
harder and requires a constant and consistent willpower to iterate, revise,
draft and edit. Please keep this in mind when editing someone's contribution.

The Checklist
=============

- ☐ The issue I am editing has been moved to ``Review in Progress`` on the
  `project dashboard
  <https://github.com/input-output-hk/hs-opt-handbook.github.io/projects/1>`_

- ☐ The issue abides by the checklist for its section of the book. `Part 1
  <https://github.com/input-output-hk/hs-opt-handbook.github.io/blob/main/Contributing/Part1.rst>`
  or `Part 2
  <https://github.com/input-output-hk/hs-opt-handbook.github.io/blob/main/Contributing/Part2.rst>`

- ☐ My comments and edit requests are actionable. Do not just say::

        This was hard to understand

  Be specific and say *why* it was hard to understand:::

        This was hard to understand because the terminology keeps changing. In
        paragraph 1 it was ``foo``, then it paragraph 4 it was ``bar`` and now
        you are using ``baz`` and each interchangeably! Please stick to one
        phrase and use it consistently throughout the piece.

- ☐ If an example is given. The example is introduced, *and* the point of the
  example is explicitly stated in the text. For example:

  Do:
      .. code-block:: haskell

         fmap :: (a -> b) -> [a] -> [b]
         fmap f []     = []
         fmap f (x:xs) = f x : fmap f xs

      Above we see the higher ordered function ``fmap``. It is a higher ordered
      function because the parameter ``f`` is a function as shown in f's type:
      ``(a -> b)``. This means that in order to use ``fmap`` we must pass into
      it a function!

  Don't:
      .. code-block:: haskell

         fmap :: (a -> b) -> [a] -> [b]
         fmap f []     = []
         fmap f (x:xs) = f x : fmap f xs

      Above we see the higher ordered function ``fmap``. It is a higher ordered
      function because it takes a function as input.

- ☐ Sentences are *concise*. That is, there are not many words in each sentence
  that *do no work*. See `here
  <https://owl.purdue.edu/owl/general_writing/academic_writing/conciseness/eliminating_words.html>`_
  for examples.

- ☐ The section defines an internal reference target, for example::

        .. _sec-four-horsemen:

        The Four Horsemen of Bad Haskell Performance
        ...
