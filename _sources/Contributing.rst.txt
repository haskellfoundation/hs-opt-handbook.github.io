:orphan:
.. _contributing-readme:

=================
How to Contribute
=================

General Procedure and Working Convention
========================================

There are many ways to contribute and each have their own contributing guide. If
you intend to contribute text then please start by reading through the `Style
Guide
<https://github.com/input-output-hk/hs-opt-handbook.github.io/blob/main/style-guide.rst>`_
. We make extensive use of checklists to track and manage state, so If you:

- Want to suggest a new section, please open an issue and we'll triage in the issue.
- Want to be an editor, please read the `Editor Contribution guide
  <https://github.com/input-output-hk/hs-opt-handbook.github.io/blob/main/Contributing/Editing.rst>`_.
- Want to be a writer. We split the writing by book topic, so:

  - Write a profiling or debugging section; see the `Measurement and Observation Contribution Guide
    <https://github.com/input-output-hk/hs-opt-handbook.github.io/blob/main/Contributing/Measurement_Observation.rst>`_.
  - Write an optimization section; see the `Optimizations Contribution Guide
    <https://github.com/input-output-hk/hs-opt-handbook.github.io/blob/main/Contributing/Optimizations.rst>`_.
  - Write a Case Study; see the `Case Study Contribution Guide
    <https://github.com/input-output-hk/hs-opt-handbook.github.io/blob/main/Contributing/CaseStudies.rst>`_.

Lastly, you might also benefit from reading the original `Haskell Foundation
proposal
<https://github.com/doyougnu/tech-proposals/blob/hs-opt-handbook/proposals/2022-01-31-haskell-optimization-handbook.md>`_.


Kinds of Contributors
---------------------

There are 2 kinds of contributors to this project:

    **Content Contributors**: Any person who opens a pull request on the book repo with the intent to contribute a section or chapter.

    **Editorial Contributors**: Any person who reviews, edits, and makes
     suggestions to a content contributors' pull request for accuracy of
     content, grammar, structure and communication quality.

We use github's issue tracking and projects to manage the work flow. The process
is this, for each part, section or subsection in the table of contents a
corresponding github issue exists in the book repository. Each issue can then
have one or more tags, where each tag tracks properties of the section; these
are:

    **Phase n**: The issue belongs to phase 0, 1, or 2. Phase's are an index to
    roughly track *when* an issue will be worked on. Almost all issues should
    now be Phase 1 or above. Phase 0 is for issues pertinent to the pre-public
    release of the book. Phase 1 is for issues that *should* be in the first
    public release, and Phase 2 is for issues that should eventually make it
    into the book, but are okay to skip for now.

    **Kind**: one of Optimization, Measurement, Case Study, etc. These are
    roughly the parts of the book as listing in the table of contents.

    **Platform**: a tag which states the target platform of the technique, for
    example: ghc-specific, linux, web-browser. This set is open ended and I'm
    open to any suggestions here, the tags are for project metrics, i.e., we had
    10 measurement sections and 15 optimization techniques. These metrics serve
    as bylines for advertising the book: "HOH now has ways to make your code
    faster!"

Project Coordination
--------------------

We use the github project dashboard, just as many projects in open source do. In
particular, the life cycle of a section (as represented by an issue) is in one
of 5 states:

    1. To do
    2. In progress
    3. Needs Review
    4. Review in progress
    5. Reviewer approved

Each state is represented by a column in the kanban project board as provided by
github. When a new contributor wants to begin working on a section they'll begin
the pipeline, which is:

    - Find the appropriate issue and ask to be assigned to the issue.

    - An Editorial Contributor assigns the issue to them and moves the issue in
      the project board to the `In Progress` column.

    - Once the contributor finishes the first draft they open a PR on the book
      repo. This triggers a project hook in github to automatically move the
      issue from `In progress` to `Needs` review.

    - Once the issue is in `Needs Review` any Editorial Contributor can review
      it. This editor *now owns* and *is expected* to shepherd the section. The
      editor reviews the PR with respect to the appropriate checklist: Part 1
      checklist for sections pertinent to Part 1 of the book, case study
      checklist for case studies etc. Then makes recommendations to the
      contributor in the PR as comments in github during a PR review. Requesting
      changes in github automatically moves the issue from `Needs Review` to
      `Review in Progress`.

    - Once the contributor applies the comments they ping the editor that
      comments have been addressed and manually moves the ticket back to Needs
      Review.

    - The editor, having received the ping, does a final review, either they
      suggest more changes moving the issue back to `Review in Progress` or they
      are satisfied and approve the PR. Approving the PR triggers a project hook
      to move the issue to `Reviewer Approved`. The PR is now mergable and the
      section is considered finished.

    - The editor then adds the contributor to the `Contributors <https://github.com/input-output-hk/hs-opt-handbook.github.io/blob/main/src/contributors.rst>`_ file.
