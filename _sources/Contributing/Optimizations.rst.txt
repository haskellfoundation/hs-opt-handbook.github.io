:orphan:

=================
How to Contribute
=================
------
Part 2
------

Introduction
============

This is a checklist for contributors of optimizations portion of the Handbook.
Please make sure that your contribution satisfies all items on the checklist. We
use checklists to check the quality of each section of the handbook. If this is
news to you then please see `the contributing guide
<https://github.com/input-output-hk/hs-opt-handbook.github.io/tree/main/Contributing.rst>`_
to get up to speed on the expected project workflow as a writer.

The Checklist
=============

- ☐ The section states what the optimization is.
- ☐ The section states how invasive the optimization is. That is, is it simply a
  flag change, does it require a source code change? If so, how much a new
  pragma? A new data structure?
- ☐ The section states why the optimization is an optimization. Provide a
  theoretical reason, such as: "With Unboxing, all code which handles this value
  performs one less pointer de-reference to the heap, thus yielding a speedup."
- ☐ The section provides evidence that the optimization *is in fact* an
  optimization. This can be Core, STG, Cmm or Assembly dumps, or a benchmark.
  Please provide a link to the relevant section (e.g., The handbook section on
  Core for Core) of the handbook for whichever one is relevant.
- ☐ The section classifies the optimization based on one of the four horsemen of
  slow code as written in the introduction. For example, "Unboxing our data
  types eliminates a good deal of pointer chasing, the second horsemen of slow
  code".
- ☐ If you read the first line of each paragraph, they are form a coherent
  story, and that story arrives at the point of the section, within reason.
- ☐ The section has a closing summary that repeats: What the optimization is,
  how invasive the optimization is, a recommendation on when to use the
  optimization, and any downsides of implementing the optimization.
- ☐ The section has a reference or further reading subsection
- ☐ The book builds with the PR (this should be CI)
