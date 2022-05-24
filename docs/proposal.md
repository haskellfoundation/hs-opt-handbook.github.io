# Haskell Optimization Handbook


## Introduction

<!-- General intro and problem statement -->
Haskell is well known as a strong statically typed, lazy, pure functional
programming language, that is able to produce programs whose performance is on
the order C. Unfortunately, writing Haskell code that is idiomatic and that
reaches C-like speeds (or better) is difficult and often out of reach for even
intermediate Haskellers. Worse still, understanding the root cause for a Haskell
programs' performance is even more difficult. For example, simply observing
performance in detail (i.e., in more detail than using the `time` utility)
requires understanding heap profiling (GHC based or not), ticky-ticky profiling
(and learning to read the ticky report), or even learning to read and understand
an intermediate language (such as Core, or STG). While none of these are
insurmountable, each one slightly increase the adoption cost of the language,
and the cognitive distance a beginner must travel should they seek to become an
expert. This proposal directly addresses these issues by consolidating diffuse,
performance knowledge into a single resource, _The Haskell Optimization
Handbook_ (HOH).

HOH's goals are similar to of other well-trodden engineering handbooks[^1], and
as such it meant for the working software developer. More specifically, HOH
seeks to be _actionable_; readers should be able to consume a relevant section,
and make progress on their issue. HOH should be _practical_; for a given
optimization method, the handbook should not only say how to implement it, but
also, when to use it and the performance issue it addresses. Similarly, for a
given measurement tool, HOH should say when to employ the tool, what responses
are characteristic of particular performance issues and if necessary, how to use
the tool. HOH should be _relevant_; case studies should come from the real
world[^2], when this is not possible example projects that demonstrate a
technique, performance regression, or tool are provided.


HOH is composed of 2 parts, roughly `Identification of a Performance Problem`,
and `Improving Runtime Performance`. Each part is further composed of several
chapters with the ordering of chapters being least invasive changes, such as
changing the garbage collection nursery size, to most invasive, such as altering
code directly, or hooking up the program in a new cabal target and investigating
with `ghc-debug`.

The scope of content should not reproduce techniques that are well-documented
elsewhere, such as the GHC user guide. In such cases, the content should be a
supporting document or a detailed case study of the technique. Similar in spirit
to the [Parsec](http://book.realworldhaskell.org/read/using-parsec.html) chapter
in Real World Haskell. In this chapter, Real World Haskell does not explain
_how_ the Parsec library works, rather it provides a practical example of _how
to use_ the library to achieve a users' goals.

To Track progress, sections and goals, this project will use github's project
dashboard. The exact workflow is in the Working Conventions section below. Since
the project is expected to be worked on asynchronously, there is a high risk of
abandonment or half finished chapters, and especially so since this is the
second performance book
[proposal](https://github.com/haskellfoundation/tech-proposals/pull/9). To
mitigate this outcome, this proposal suggests small incentives such as github
badges awarded to contributors by the Haskell Foundation. We hope to mitigate
the risk of unfinished sections or chapters through these incentives and
minimizing the barrier to entry via a clear roadmap, clear contributing guides, and a contribution checklist for each part of the book.

Lastly, as a digital book, HOH is a _living_ document. As soon as a section is
merged into the working repository that information can be rendered and made
available to Haskell developers around the world. As such, and due to the nature
of community contributions, specifying a timeline of deliverables is
challenging. However, our intention is to have Phase 1 of the project plan
finished or nearly done by the second quarter of 2022.

## Motivation

Currently, there is no clear path for working Haskell developers to learn how to
write high performance Haskell code. Instead, performance improvement techniques
are left to anecdotes spread across the internet, buried in technical
documentation, or in comments in source code. This creates several second order
effects in the community: it increases the barrier for Haskell's adoption into
industrial settings, it increases the difficulty in becoming an expert in
Haskell, and it leads to slower code that is allowed to proliferate through the
community and library ecosystem.

HOH directly addresses these problem: it consolidates resources to a single
available handbook, thereby relieving developers from searching the web and
lowering the barrier to adoption. Through this consolidation, it centralizes
performance related discussions in the community and therefore makes available
techniques that were once left to the functional programming wizards. In other
words, it provides a source of common ground for performance tips, discussions,
and resources. Thus, improving the quality of the Haskell ecosystem and driving
adoption for Haskell.

## Goals

HOH is:

 - a freely available online book
 - is _actionable_: A reader can read a section and apply a technique to make progress for their problem.
 - is _relevant_: Case studies are not manufactured examples, rather they originate from real world code.
 - is _practical_: Content that describes a performance technique describes how
   to implement it, when to use it, and its observable effect on a code base.
   Content that describes a method of analysis describes what the method is, how
   to use the method, what the perceived results of the method should be and
   relevant signals in the output of the method. For example, the GHC heap
   profiling section should describe what heap profiling is, how to do the heap
   profiling, what the output should look like, and most importantly a gallery
   of examples of poor performing heap profiles and an explanation of why they
   are poor performing.
 - Community driven
 - Maintained and updated over time, with supported versions of GHC beginning at 8.10.x (for DWARF symbols)
 - Understandable for intermediate to expert Haskell developers.

HOH has:
 - Content
   - A set of methods to retrieve reliable measurements.
   - A set of methods to characterize the runtime performance of Haskell programs.
   - A set of methods to improve the runtime performance of Haskell programs.
 - Recommendations: 
   - A set of "best-practice" characterization tools for analyzing the behavior
     of a Haskell program.
   - A set of recommendations for **idomatic** Haskell code that is performant
   for most use cases, including standard libraries to choose from, with a list
   of alternatives for special cases.

## Non-Goals

HOH will not have:
 - Content suitable for beginner functional programmers.
 - Explanations of project setup, build tools, using or setting up a shell or
   deployment of any kind, instructions on installing any third party tools for
   a given platform.
 - Descriptions, analyses and explanations of functional algorithms or data
   structures. Content will instead be "Try unordered-containers if you have
   foo, bar, baz", rather than "This is what a bankers queue or HAMT is ...".
 - A monad or monad transformer tutorial. This is assumed knowledge in the
   audience.

## Resources

This project does not require funding, and is a volunteer effort. Support from
the Haskell Foundation should be:
 - Advertisements on Haskell-related media.
 - Github badges for contributors. I believe this could be a major driver of
   contributions. I want a certified Haskell Foundation badge that can be given
   to anyone who contributes at least one pull request, even if that pull
   request is just fixing some language or references. I believe this would
   incentives people to contribute because it is an auditable token on an
   account that is likely to be reviewed by a recruiter or otherwise interested
   party. To put it simply, it is a small thing but people like baubles. We
   might even have a hierarchy of badges for frequent contributors or editors.
 - High level suggestions for high value contributions. Suggestions such as
   "From surveys we find a lot of developers struggle with performance around
   feature foo, or library bar". This item could then be discussed in this
   proposal and added to the table of contents and project plan.
 - Design resources for graphics and overall look for feel. This doesn't have to
   be direct support; we could try to crowd source it similar to crowdsourcing
   the haskell logo in 2009.

## People

This section is partly covered in the working conventions section, but I'll make
the people part more clear here:

-   **Performers:** I (Jeffrey Young (doyougnu)) will shepherd this project and
    can spend 1 hour per work day on it. I expect Performers to fulfill the role
    of Contributing Editors in the Working Conventions section below.
    Contributing Editors will be added if they are in good standing with the
    community and sufficiently motivated, or as requested by the HF, or if they
    have successfully merged at least two sections.

-   **Reviewers:** These are the same people as the Performers with the addition
    of the Haskell Foundation.

-   **Stakeholders:** We have internal stakeholders at IOHK/IOG, such as the
    plutus core team. I welcome external stakeholders such as the Haskell
    Foundation or any other invested party. It is imperative the handbook be
    useful to the working Haskell developer!

## Lifecycle:

-   **Implementation:** See Project plan and Working Conventions.

-   **Compatibility issues:** There is a risk of targetting out of date GHC
    releases. For this project we will limit the supported GHC versions to
    8.10.* and onward.

## Deliverables

The project plan proceeds from the table of contents; which is the single source
of truth for content in the book and orients the critical path. For example,
should someone suggest a new section; in the domain of this project that means
adding it to the table of contents and opening a tracking issue for it. 

Here is the table of contents that I have as a working draft:

```
Part 0: Preliminaries
1. Who this book is for.
2. What to expect. 
3. What makes Haskell programs slow down (in order of importance)
3.1 Doing more work than necessary <=> too much allocation
3.2 Fusion and why it sometimes fails

Part 1: Measurement, Profiling, and Observation
1a. Constructing a hypothesis (The code is <something> due to <something>) (or in other words what are we optimizing for?)
1b. Reliably measuring performance, observing distributions of time or
allocations: disable frequency scaling, ALSR, pinning dunique-increment and other GHC flags etc.

1.1 Heap Profiling and Inspection: GHC based Methods
1.1.1 heap profiling (most importantly _what_ to look for, e.g., classic
triangle implies memory leak): -hT -hC -hy etc.
1.1.2 eventlog2html
1.1.3 Csaba's external STG interpreter
1.1.4 ghc-debug
1.1.5 threadscope or other scopes?
1.1.6 ghc-vis

1.2 Heap Profiling and Inspection: Third Party methods
1.2.1 Valgrind+Massif

1.2 Measurement: GHC based Methods
1.2.1 command line flags, building with -prof, associated libraries for data exploration:
 - ghc-prof
 - profiteur
 - viewprof
 - profiterole
1.2.2 -dtimings flag and parsing its output
1.2.3 ticky ticky profiling
1.2.4 Niel Mitchell's reduced stack method

1.3 Measurement: Third Party Methods
1.3.1 Linux time utility
1.3.2 Linux perf, which kernel events to look for, interpreting the results.
Explanation of cycles, instruction counts, stalled cycles front end, stalled
backend.
1.3.3 Valgrind+Cachegrind
1.3.4 dtrace
1.3.5 flamegraphs
1.3.4 other low level tools: qprof, oprofile, PAPI, heaptrack?

1.4 Measurement: Haskell Libraries
1.4 Haskell libraries
1.4.1 criterion
1.4.2 gauge
1.4.3 tasty-bench
1.4.4 weigh
1.4.5 inspection-testing
1.4.6 nothunks
1.4.7 others?

1.5 Direct Observation: Reading Core (and relating core to program behavior in detail)
1.5.1 Preliminaries: z-encoding, dumping and navigating core 
1.5.2 What allocation in Core looks like
1.5.3 Bad patterns in Core
1.5.4 Good patterns in Core
1.5.5 Demand analysis
1.5.6 Simplifier data in Core
1.5.7 Occurrence analysis 
1.5.8 Others?

Part 2: Optimizations

Part 2.1: GHC based techniques 
2.1.1: Garbage collector tweaks
2.1.2: Fine grained optimization flags: -qg increase GC cradle size etc.
2.1.3: other ideas?

Part 2.2: Library agnostic code changes
2.2.1 Unboxing
2.2.2 Unpacking Product types (Sum types are
[speculative](https://gitlab.haskell.org/ghc/ghc/-/wikis/unpacked-sum-types) see
[^1](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4140))
2.2.3 Specialization: Too much abstraction destroys inlining and fusing (add Specializations)
2.2.4 Fusing and Rules pragmas
2.2.5 Using NOINLINE, INLINE, INLINABLE
2.2.6 foldr vs foldl and identifying if tail call optimization fire, or shortcircuiting with foldr
2.2.7 Wrangling lazy tuples (tuple is evaluated to WHNF, thunks remain internal to tuple)
2.2.8 GHC.Exts for very low level hot code. For example, swap PrimArray for STRefs
2.2.9 Re-order constructors for common use cases (case study of IntMap here)
2.2.10 loopification
2.2.11 manual worker wrapper
2.2.12 Deeply nested monad transformers in a hot loop (ExceptT, ReaderT, StateT)
2.2.13 Parse don't validate to remove runtime checks (do less!).
2.2.14 Removing monad transformer stacks: 
 - ReaderT, replace with `reflection` or `ImplicitParams`. 
 - Remove StateT for STRefs IORefs
 - Remove ExceptT, then throw/catch in IO instead
2.2.15 OneShot monad trick
2.2.16 Continuation Passing Style
2.2.17 Choosing better data types in general
2.2.18 ...other black magic... Data Families? Backpack? GADTs?
```

## Project Plan

### Phase 0: Initialization
This is an initialization phase, its purpose is to get the project up and
running and ready for external contributions. Note that the last item of each
phases' checklist is a community check-in. Phase0 is completed when:

 - [ ] : Working repository for the book is setup.
 - [ ] : Continuous integration for the book is setup and the book builds for
       every commit to main.
 - [ ] : Github project is setup.
 - [ ] : Contribution guide for sections in Part 1 is complete.
 - [ ] : Contribution guide for sections in Part 2 is complete.
 - [ ] : Part 1 section checklist is complete.
 - [ ] : Part 2 section checklist is complete.
 - [ ] : Case study checklist is complete.
 - [ ] : Three sections of Part 1 (Measurement) are complete.
 - [ ] : Three section of Part 2 (Optimizations) are complete.
 - [ ] : One case study is complete.
 - [ ] : List of contributors is initialized.
 - [ ] : Community check in has been done on Discourse.Haskell after each other
       bullet is complete

The deliverables for this phase constitute examples for other contributors, so
that when and if an external contributor tries to contribute they have something
to work off of rather than a blank page. In particular they have checklists to
work off of and example sections to refer to. This lowers the barrier of entry
for new contributors and creates a positive reinforcement loop as the book
progresses. An example checklist for an external tooling section could be:
```md
- [ ] provide the motivation for using the external tool
- [ ] provide a phrase "use this tool when ..."
- [ ] provide the expected output of the tool "With this tool you can retrieve this important information"
- [ ] provide the platforms that support this tool, if a platform does not
  please state either: 1) an alternate tool to retrieve the same information or 2) a statement that this tool is not supported on the platform
- [ ] if you read the first line of each paragraph, they are form a coherent story
  , and that story arrives at the point of the section, within reason.
- [ ] The section has a closing summary that repeats: when to use the tool, what information can be retreived with the tool, which performance issues this tool helps with, and how to run/use the tool.
- [ ] After the summary there is a summary of command line invocations and their corresponding meaning
- [ ] The section has a reference subsection
- [ ] The book builds with the PR (this should be CI)
```

### Phase 1: Measurement and Observation
The goal of this phase is two fold. First, finish a section on reading modern
core, complete with a description of all the information that is usually
stripped out of the "Reading Core" tutorials. Why? Because reading and
understanding Core is a major barrier to performance optimizations, it is what
the experts do, what GHC perceives, and can help diagnose large classes of
performance-related behaviors. Second, a section on attaining reliable
benchmarks on Linux (this could be expanded to other platforms, but per the
Haskell Survey the vast majority of Haskell developers work on and target
Linux). This phase is complete when:

 - [ ] : Section on reading Core is complete
   - [ ] : A description of what allocation in Core is complete.
   - [ ] : A description of demand analysis information in Core is complete.
   - [ ] : A description of the simplifyer information in Core is complete.
   - [ ] : A description of the occurrence analysis information in Core is complete.
   - [ ] : Descriptions of poor performance patterns, and a description of why
         they are poor performing, in Core is complete.
 - [ ] : Section on how to reliably benchmark on Linux is complete.
 - [ ] : Section on benchmarking utilities is complete, Specifically these
       libraries and tools:
   - [ ] : (Haskell Library) Criterion
   - [ ] : (Haskell Library) Gauge
   - [ ] : (Haskell Library) Tasty-Bench
   - [ ] : (Haskell Library) Weigh
   - [ ] : (Utility) Perf
   - [ ] : (Utility) Cachegrind
 - [ ] : three more sections on heap profiling in Part 1 are complete.
 - [ ] : three more sections of Part 2 are complete and these sections refer to
       either the Core reading guide or the benchmarking section.
 - [ ] : Two case studies are complete and show real world poor performance
       patterns in Core, and associated improvements in benchmarking data.
 - [ ] : Community check in has been done on Discourse.Haskell after each other
       bullet is complete

### Phase 2: Coasting
By phase 2 we should have good working conventions setup and numerous example
chapters for new contributors. This phase is complete when for each item in the
table of contents a corresponding chapter has been merged into main. Thus
constituting the first feature complete draft of the book. If necessary, at this
phase the project can regroup and define subsequent phases for additional
material or extra parts.

## Timeline

By phase:
 - Phase 0 (1-2 months): Most of this phase is simply setup with example
   chapters. I estimate 1 week per section. This should be an over estimate as
   the sections I have in mind are already written as blog posts or GHC wiki
   pages. For example, detailed use of `cachegrind` to profile Haskell programs,
   or heap profiling a known memory leak in the `sbv` library[^3].

 - Phase 1 (3-4 months): This is a more meaty phase and so I allot more time for
   it. The major goal of this phase is a detailed account of reading `Core`, I
   have a decent amount of experience reading Core, but getting all the nuances
   and information correct will take time and I'll have to refer to numerous
   papers to do so. Similarly, finding, diagnosing and documenting case studies
   will take even more time. Thus I want to allot 2x the time of phase 0 for
   phase 1 with a priority on the `Core` documentation and finding case studies.

 - Phase 2 (4-6 months): This phase is the furthest out into the future and thus
   has the most noise in its time estimate. The timeline for this phase depends
   on how many contributors the project acquires, how easily those contributors
   contribute and if the scope of the project (i.e., the table of contents)
   increases. This is my best guess, my worst case guess would be 8-10 months.
   However, I would like to note that the project risk in this phase is lower
   since the book may already be useful for its intended audience once this
   phase has begun.

## Working Conventions
Let's start with the types. There are 2 kinds of contributors to this project:

 1. Content Contributors: Any person who opens a pull request on the book repo
    with the intent to contribute a section or chapter.
 2. Editorial Contributors: Any person who reviews, edits, and makes suggestions
    to a content contributors' pull request for accuracy of content, grammar,
    structure and communication quality.

We'll use github's issue tracking and projects to manage the work flow. So for
each part, section or subsection in the table of contents a corresponding github
issue exists in the book repository. Each issue can then have one or more tags,
where each tag tracks properties of the section; these are:

 - phase `n`: The issue belongs to phase 0, 1, or 2.
 - kind: either `Optimization`, `Measurement`, `Case Study`, `Misc`.
 - platform: a tag which states the target platform of the technique, for
   example: `ghc-specific`, `linux`, `web-browser`. This set is open ended and
   I'm open to any suggestions here, the tags are for project metrics, i.e., we
   had 10 measurement sections and 15 optimization techniques. These metrics
   serve as bylines for advertising the book: "HOH now has <n> ways to make your
   code faster!"

Now to coordinate the project we'll use the github project dashboard, just as
many projects in open source do. In particular, the life cycle of a section (as
represented by an issue) is in one of 5 states:
 1. To do
 2. In progress
 3. Needs Review
 4. Review in progress
 5. Reviewer approved

Where each state is represented by a column in the kanban project board as
provided by github. When a new contributor wants to begin working on a section
they'll:
 - Find the appropriate issue and ask to be assigned to the issue.
 - An Editorial Contributor then assigns the issue to them and moves the issue
   in the project board to the `In progress` column.
 - Once the contributor finishes the first draft they open a PR on the book
   repo. This triggers a project hook in github to automatically move the issue
   from `In progress` to `Needs review`.
 - Once the issue is in `Needs Review` any Editorial Contributor can review it.
   This editor now owns and is expected to shepherd the section. The editor
   reviews the PR with respect to the appropriate checklist: part 1 for sections
   pertinent to part 1 of the book, case study checklist for case studies etc.
   Then makes recommendations to the contributor in the PR as comments in github
   on the contributed text. Requesting changes in github automatically moves the
   issue from `Needs Review` to `Review in Progress`.
 - Once the contributor applies the comments they ping the editor that comments
   have been addressed and manually moves the ticket back to `Needs Review`.
 - The editor, having received the ping, does a final review, either they
   suggest more changes moving the issue back to `Review in Progress` or they
   are satisfied and approve the PR. Approving the PR triggers a project hook to
   move the issue to `Reviewer Approved`. The PR is now mergable and the section
   is considered finished.

HOH will be a living document thus after each pull request continuous
integration will rebuild the book and update the version on the github host.

## Outcomes

1. a freely available book hosted on Github
2. A repository containing the content of the book, written in markdown
3. Content covering optimization techniques, measurement techniques, and general
   performance guidelines for Haskell programs.


## Risks

There are three major risks:

    1. We risk lack of contributions after some time. Writing is hard, writing
       to effectively is even harder. We mitigate this risk through the working
       conventions and use of checklists to provide clear indications of incremental progress.
    2. The second major risk is the section on reading Core with a performance
       mindset. This risk is tougher to mitigate since it depends on deep
       knowledge of numerous aspects of GHC. This risk is only mitigated through
       crowd sourcing knowledge that I (Jeff) do not have.
    3. The last major risk is finding good case studies for the book. This is a
       major risk because it contains the most unknowns on the critical path for
       this project. Again, crowd sourcing and reading through performance
       patches is the way to mitigate this risk, however it is entirely possible
       that some case studies will be too esoteric to be communicative. In such
       a case, we will violate the relevancy maxim of the book and use a simpler
       to understand artificial example.

[^1]: https://www.itl.nist.gov/div898/handbook/

[^2]: There are several great examples I already have in mind. Including but not
    limited to a major memory leak in the sbv library due to its caching
    mechanism (an IORef is storing a thunk!), the state monad transformer stack
    in GHC's LLVM backend (and really that entire backend is unoptimized).
    Failure to not encode constraints in the type system, which subsequently
    leads to a lot of boolean checks with guards in GHC. See GHC issue #20730.

[^3]: https://github.com/LeventErkok/sbv/issues/572#issuecomment-751982128
