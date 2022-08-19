:orphan:

==================
HOH Style Guide
==================

Prose
=====

- Prefer title case for chapter/section headings, ex: `Discovering a Memory
  Leak` rather than `Discovering a memory leak`.
- We use the `Python Style Guide
  <https://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html#sections>`_
  for organizing section headers. That is::

    # with overline, for parts

    * with overline, for chapters

    = for sections

    - for subsections

    ^ for subsubsections

    " for paragraphs



- Prefer italics over single quotes when calling out a term, ex: `is a
  *pure function*` rather than `is a ‘pure function’`.
- Hard wrap at 80 chars
- Prefer not mixing code and not-code in one word, ex: ``Remember when we
  `import Data.Map.Lazy?``` rather than ``Remember when we `import`d
  `Data.Map.Lazy?``

Code
====

- Add the file name before markdown blocks to make it clear which file we're
  talking about, when applicable.
- When making changes to code, make it clear which parts of the code changed and
  which stayed the same. Point this out explicitly in the text, for example::

    Our original function call was:
      ``map (\y -> let x = ...expensive... in y + x) xs``.
    Notice that we have a ``let-binding`` that will allocate a let *for each* element in ``xs``.
    We might considering floating out the let, yielding::
      let x = ...expensive...
        in map (\y -> y + x) xs
    Now we see that we have only a _single_ let that is then reused *for each* element in ``xs``,
    because we have moved the let outside of the call to ``map``.

  In this example we specifically point out the object of our change; the *let
  expression*, the way we changed it; *floating it out of the lambda*, and the
  impact of the change; a *single reference* to a single let rather than a new
  let for each element.
- Split up long lines as appropriate to keep them under 80 chars if possible
  Use ``bash`` syntax highlighting for command line output code blocks

Links
=====

Once all the scripts are done:

- If a link shouldn't be printed, mark it to be ignored.
  * This includes all "Chapter XX" intra-book links, which *should* be links for the HTML version
- Make intra-book links and stdlib API doc links relative so they work whether
  the book is read offline or online.


Reference
=========
This guide copied and modified with respect from
[rust-lang/book](https://github.com/rust-lang/book). Thank you rust community!
