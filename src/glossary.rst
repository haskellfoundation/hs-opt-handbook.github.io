.. _glossary:

Glossary
========

.. glossary::

   Algebraic Data Type

      First implemented in the Hope programming language
      :cite:t:`historyOfHaskell`, Algebraic Data Types are *composite*, meaning
      that they are data types made from other data types. For example,

      .. code-block:: haskell

         data Foo = One Int Int
                  | Two Bool Int
                  | Three Float Bool Char

      Here ``Foo`` is an algebraic data type made from the types ``Int``,
      ``Bool``, ``Float``, and ``Char``. In general, these data types are
      composed from *product types* and *sum types*.

      Product types are the types on finds in most other imperative and object
      oriented programming languages, such as ``struct`` in C or tuples in
      Haskell. Product types are called so because they form the cartesian
      product on the set of elements represented by the type, for example the
      type ``(Int, Bool)`` would be written :math:`Int \times Bool`, and would
      represent the set of all pairs of elements of the sets represented by the
      types ``Int`` and ``Bool``.

      Sum types are often not found in imperative and object oriented languages,
      but are a *tagged union* or *disjoint union* of other types. Again,
      thinking in terms of sets, a sum type represents a disjoint union of two
      or more types. For example ``Foo`` is the union of three product types
      that are each *tagged* with a constructor: ``One``, ``Two`` and ``Three``.
      Thus in terms of sets on might write the type ``Foo`` as :math:`f \in Foo
      = (Int \times Int) + (Bool \times Int) + (Float \times Bool \times Char)`.
      Notice also that the type ``Foo`` with elements ``f`` are *structural* (by
      its definition we know a ``Foo`` can only be a ``One``, ``Two``, or
      ``Three`` and how many fields each of these constructors have) as opposed
      to *nominal*. Nominal types are the kind of types created by a
      ``newtype``. They can have the same *representation* but are treated as a
      wholly unique type.

      .. admonition:: Help Wanted
         :class: help-wanted

         I've tried to give a thorough description of algebraic data types
         without diving into too much type theory, but I find the explanation a
         bit unsatisfying. For example, it is not clear *why* these are called
         ``Inductive`` or ``Algebraic`` because I deemed this was too much depth
         for a glossary entry. If you have a good resource or would like to take
         a stab at this entry then please make an issue and have at it!

   Arity

      The arity of a function is the number of arguments the function must take
      to conclude to a result.

   Atomic : Type

      In type theory, an atomic type, also sometimes called a base type. is a
      type that is not divisible because it contains no internal structure. For
      example, a tuple in not atomic because *has* an internal structure because
      it is a composition of two other types: the ``fst`` and ``snd`` elements,
      and we can *decompose* it without knowing anything about those elements
      via the ``fst`` and ``snd`` projections. In contrast, ``Int``, ``Float``,
      ``Bool``, ``Char``, and ``String`` are atomic because they are not the
      composition of other types, they are simply sets of unstructured values.
      See :cite:t:`TAPL` Section 11.1 for more.

      Note that this is from a *theoretical* perspective. From an
      *implementation* perspective these types *do* have structure, for example
      ``String`` implemented as a list of characters and a ``Float`` is
      implemented in memory as a bitvector with three fields: a sign bit, a set
      of bits for the exponent, and a set of bits for the fraction (in `IEEE 754
      <https://en.wikipedia.org/wiki/IEEE_754>`__).

   Boxed : Levity

      A Boxed value is a value that is represented by a pointer to the heap. For
      example, a value such as ``1729 :: Int`` is defined as:

      .. code-block:: haskell

         -- in GHC.Types in the ghc-prim library
         -- ...
         -- | A fixed-precision integer type with at least the range @[-2^29 .. 2^29-1]@.
         -- The exact range for a given implementation can be determined by using
         -- 'Prelude.minBound' and 'Prelude.maxBound' from the 'Prelude.Bounded' class.
         data Int = I# Int#


      and is represented in memory as:

      .. tikz::
         :libs: shapes, arrows.meta, positioning
         :align: center

         \begin{tikzpicture}[
            node distance=50pt and 1.5cm,
            data/.style={draw, minimum width=1.5cm, minimum height=2cm},
            pointer/.style={draw, minimum width=1cm, minimum height=2cm,-{Stealth[scale=2]}},
            dot/.style={circle, fill, inner sep=1pt}
         ]

         % Nodes off the linked list
         \node[data] (cons) {I\#};
         \node[pointer, right= -\the\pgflinewidth of cons.east] (p1) {};
         \node[data, right=of p1] (int) {1729\# :: Int\#};

         % Pointers (arrows) between nodes
         \draw[pointer] (p1.center)  -- (int.west);
         \draw[dot]     (p1.center) circle (3pt);

         \end{tikzpicture}

      the *box* is the ``I#`` constructor because it "boxes" the payload with a
      pointer (represented as an arrow). The payload is a heap object that is an
      :term:`unboxed` type ``Int#``, which in this case, is the unboxed literal
      ``1729#``.

   CAF

     A CAF, or Constant Applicative Form, is a Haskell value which contains no
     free variables and is not a function. Consider these examples:

     .. code-block:: haskell

        -- these are CAFs
        -- A static literal is a CAF
        foo :: Int
        foo = 12

        -- A reducible expression that requires no input is a CAF
        bar :: (Int, [Int])
        bar = ((*) 10 10, [1..])

        -- not a lambda, curried functions that can be reduced when given an
        -- input are CAFs
        baz :: Int -> Int
        baz = (*) 3

        -- not CAFs
        qux :: Int -> Int
        qux e = e * 3     -- equivalent to baz but is a lambda so not a CAF

        quux :: Int -> Int
        quux = (*) x      -- x is free thus not a CAF

     These values are *constant* because they don't bind any variables or have
     any free variables. Because they are constant they are floated (see
     :term:`Let Floating`) to the top of the program, and statically allocated
     during compile time. Since they are statically allocated at compile time
     CAFs are pinned memory and special treatment in the runtime system. Thus,
     heavily allocating CAFs can increase memory residency. See
     :cite:t:`jones1992implementing` Section 10.8 for more details.

   Cardinality Analysis

      A static analysis that GHC performs to determine: (1) How many times a
      lambda-expression is called, (2) Which components of a data structure are
      never evaluated, (3) How many times a particular thunk is evaluated. See
      :cite:t:`callArityVsDemandAnalysis` and :cite:t:`hoCardinality` for more.

   Compound Types

      Compound type are another name for an :term:`algebraic data type`. We
      refer the reader to that entry.


   Closure

      A closure is value that pairs a function with an environment, where the
      environment maps every free variable in the function with a value or
      reference to which the free variable was bound when the closure was
      created. Closure's are the canonical way to realize lexical scoping in
      languages with first-class functions, such a Haskell. See `the wikipedia
      <https://en.wikipedia.org/wiki/Closure_(computer_programming)>`_ entry for
      more.

   Closure Conversion

      Closure conversion is the default way GHC treats free variables in a
      function body. Closure Conversion creates a top level record for the
      original function, called the function environment, whose fields are the
      free variables of the function. The environment is passed to the function
      as an implicit parameter and the free variable call sites are rewritten as
      field accesses. Then the function and the record are grouped in a tuple,
      i.e., a closure (pair of environment and function) is created causing some
      extra heap allocation. Finally the call sites of the original function are
      rewritten to pass the environment with the original function. Consider
      this example:

      .. code-block:: haskell

         ...
         let f = foldl (\acc _ -> acc + x) y xs
         in  f [1..100]
         ...

      In this example ``x`` and ``y`` are free variables in the function ``f`` .
      Closure conversion will capture them and transform this function to:

      .. code-block:: haskell

         ...
         -- the function environment
         data EnvF = EnvF { x :: Int, y :: Int }

         -- the new function
         f_cc env xs = foldl (\acc _ -> acc + x env) (y env) xs

         -- the closure that replaces the original function in the same scope
         let f = (f_cc, EnvF x y)
         in (fst f) (snd f) [1..100]
         ...

      Notice closure conversion has *added* an extra ``let`` expression for the
      closure and the reference to ``x`` and ``y`` have been replaced with
      accesses to ``env`` . The let expression can be a source of extra heap
      allocations and is one of the costs of closure conversion. However, the
      benefits are uniformity; every function can be treated as a closure.
      Closure conversion is often contrasted with Lambda Lifting which is
      another strategy to handle free variables that does not incur extra heap
      allocation. See :cite:t:`lambdaLifting` and
      :cite:t:`selectiveLambdaLifting` for more.

   DWARF : Format

      DWARF symbols are a widely used and standardized data format used to
      provide source level debugging. For more, see `the official webpage
      <https://dwarfstd.org/>`_.

   Entry Code

      The entry code for a closure on the heap is the code that will evaluate
      that closure. There are some nuances and exceptions: For functions the
      entry code applies the function to its arguments, which the entry code
      assumes are all present; that is, the entry code assumes all arguments are
      either loaded into registers or are already on the stack. Should the
      function be applied to too few arguments or should the function be an
      :term:`Unknown function` then a generic apply is used. For a :term:`PAP`,
      there is no entry code. PAPs can only be applied to more arguments using
      the generic apply functions. Lastly, :term:`Unlifted` Objects cannot be
      evaluated and thus have no entry code.

   Full Laziness transformation : Optimization

      A form of :term:`Let Floating` which moves let bindings out of lambda
      abstractions to avoid unnecessary allocation and computation. See
      :cite:t:`peytonjones1997a` Section 7.2.

   Fusion : Optimization

      See :ref:`What is Fusion <canonical-fusion>`.

   HNF : Normal Forms

      An expression that is in *head normal form* is a value which contains at
      least one :term:`thunk`. If the value does not contain any thunks, then it
      is said to be in normal form (:term:`NF`). See
      :cite:t:`jones1992implementing` Section 3.1 for more.

   Info Table : Runtime

      Every heap allocated object in the runtime system keeps an information
      table that stores data such as: the object type (function, data
      constructor, thunk etc.) before the payload of the object. This is called
      the Info Table. See :cite:t:`pointerTaggingLaziness`, :ghcWiki:`wiki
      <commentary/rts/storage/heap-objects#info-tables>`, and
      :cite:t:`SpinelessTaglessGMachine` Section 7.1 for more details.

   Info Table Address : Runtime

      The memory address for heap object descriptors :term:`info table`.

   Join Point : Optimization

      A join point is a place where different execution paths come together or
      *join*. Consider this example slightly modified from
      :cite:t:`compilingWithoutCont`:

      .. code-block:: haskell

         let join1 _ = some_large_expression
             join2 _ = some_other_large_expr
         in if e1 then (if e2 then join1 () else join2 ())
                  else (if e3 then join1 () else join2 ())

      In this example, ``join1`` and ``join2`` are join points because the
      branches described by each if-expression conclude by calling them. Thus,
      the control flow described by the if-expressions joins at specifically
      ``join1`` and ``join2``. Join points are an important optimization
      technique that GHC performs automatically to remove redundant allocations.
      Had we not wrapped ``some_large_expression`` and ``some_other_large_expr``
      in a ``let``, then these expressions would be duplicated *and* would be
      captured in an additionally allocated closure unnecessarily. Join points
      avoid these problems and are particularly relevant for Stream
      :term:`Fusion` performance. For more see the join points paper:
      :cite:t:`compilingWithoutCont`.

   Known Function

     A known function is a function in the STG machine of which GHC statically
     knows the :term:`Entry Code` pointer and the :term:`Arity` of. This means
     that the function binding site is statically visible, that is, the
     function is :term:`Top-Level`, or the function is bound by an enclosing
     ``let``. With this information the STG machine can use a faster function
     application procedure because the function pointer does not need to be
     scrutinized. See also :term:`Unknown Function`.

   Levity Polymorphism

      A kind of polymorphism that abstracts over calling conventions which
      allows levity polymorphic functions to be abstracted over memory layout.
      See :cite:t:`levityPolymorphism` for a more precise technical definition
      and discussion.


   Let Floating : Optimization

      A group of optimizing transformation's that move ``let`` bindings to
      reduce heap allocations. See :cite:t:`partain1996let-floating` and
      :cite:t:`peytonjones1997a` Section 7 for more details.

   Lifted : Levity

      A Lifted type is a type that contains the value :math:`\bot`; which means
      the type is lazy and capable of representing non-terminating computation.
      For example, the ``Bool`` type is a set with three values: ``True``,
      ``False``, and :math:`\bot`. Therefore ``Bool`` is a Lifted type.

   Loop Fusion

      Loop fusion is a classic optimization technique that reduces the number of
      loops in a program, thereby reducing the number of memory accesses and the
      number of looping constructs. In Haskell, loop fusion transforms many
      traversals over the same data structure to a single traversal. A classic
      example of this is map fusion.

      .. code-block:: haskell

         -- two traversals, one for f, one for g on the result of f
         map g . map f $ [1..100]

         -- after map fusion:
         -- only one traversal
         map (g . f) [1..100]

      This can also appear in list comprehensions, for example:

      .. code-block:: haskell

         ...
         -- three traversals: two to project elements, 1 to fold
         let foo = foldl + 0 [ i | (i,_) <- args ]
         let res = bar foo   [ j | (_,j) <- args ]

         -- after loop fusion on the list comprehensions
         -- 2 traversals: one for the arguments, one to fold
         let (is, js) = unzip args
         let foo = foldl + 0 is
         let bar = bar foo js

   Multi-Shot Lambda

      A multi-shot lambda is a lambda that is called *more* than once. In
      contrast to a :term:`one-shot lambda`, a multi-shot lambda has a high risk
      of destroying :term:`sharing` if subject to certain optimizations, such as
      Inlining. GHC determines whether a lambda is one-shot or multi-shot during
      :term:`Cardinality Analysis`. See :cite:t:`hoCardinality` and
      :cite:t:`callArityVsDemandAnalysis` for more.

   NF : Normal Forms

      An expression that is in *normal form* is a fully evaluated expression and
      is a value which contains no thunks. This is in contrast to weak head
      normal form (:term:`WHNF`) and head normal form (:term:`HNF`), both of
      which may contain thunks. See :cite:t:`jones1992implementing` Section 3.1
      for more.

   Occurrence Name : GHC

     An Occurrence name is a name GHC assigns to an entity to disambiguate
     multiple occurrences of that name. Disambiguation allows GHC to distinguish
     *by name* a type constructor from a data constructor, which often occurs
     due to punning, or from local variables in separate functions with the same
     name, such as ``x`` or ``xs``. Occurrence names are a pair of the original
     name (as a ``FastString``, a GHC internal type) and a ``NameSpace``; they
     are ubiquitous in GHC and in the intermediate representations. For example,
     the occurrence name for the function ``f x y = ...`` will be similar to
     ``f_r17p``. The exact occurrence name will change, but parts are static.
     For example, the ``f`` before the underscore always comes from the name of
     the function. Had ``f`` been name ``fancyFunction`` then the ocurrence name
     would have been ``fancyFunction_r17p``. Similarly, leading character in the
     suffix; the ``r`` in ``r17p`` is static and meaningful. In this case, the
     ``r`` indicates that the name ``f`` is an element in the ``NameCache``,
     meaning that all references to ``f`` share a single ``Unique`` ID in every
     GHC invocation (See the :ghcSource:`Note [The Name Cache]
     <compiler/GHC/Types/Name/Cache.hs?ref_type=heads#L36>` for more). When
     occurrence names are generated, the leading character is a hint for what
     kind of name is being generated. You can find an incomplete list of tags
     and their meanings in :ghcSource:`Note [Uniques for wired-in prelude things
     and known tags] <compiler/GHC/Builtin/Uniques.hs?ref_type=heads#L305>`. For
     more on names see :ghcSource:`Note [Choosing external Ids]
     <compiler/GHC/Iface/Tidy.hs?ref_type=heads#L271>` and `this
     <https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/rdr-name-type#the-occname-type>`__
     wiki page on GHC's Reader names.

   One-Shot Lambda

      A one-shot lambda is a lambda that is called *exactly* once. These
      lambda's are common in functional programming and can be subject to more
      aggressive optimizations due to their one-shot nature. For example, there
      is no risk of losing :term:`sharing` in a one-shot lambda as a result of
      inlining free variables or floating let expressions *into* the lambda;
      something that GHC usually avoids. See :cite:t:`hoCardinality` and
      :cite:t:`callArityVsDemandAnalysis` for more background. See the magic
      `oneShot
      <https://hackage.haskell.org/package/base-4.17.0.0/docs/GHC-Exts.html#v:oneShot>`_
      function in `GHC.Exts
      <https://hackage.haskell.org/package/base-4.17.0.0/docs/GHC-Exts.html>`_
      for an unsafe way to instruct GHC that you have a one-shot lambda.

   PAP

      A PAP is a partial application. PAPs are heap objects and thus a type of
      closure that represents a function applied to *too few* arguments. PAPs
      should never be entered, and are only applied using the generic apply
      functions in the STG machine. See the file ``rts/Apply.cmm`` in GHC or the
      :ghcWiki:`heap object <commentary/rts/storage/heap-objects>` wiki page for
      more.

   Pinned : Memory

     Pinned memory is memory that is guaranteed to not be moved by GHC's garbage
     collector. This is most often useful for interfacing with foreign code.
     Note that pinned memory may lead to memory fragmentation and increased slop
     because it never moves. See `Well Typed's
     <https://well-typed.com/blog/2020/08/memory-fragmentation/>`_ post and the
     `wiki
     <https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/gc/pinned>`_
     for more.

   Reproducer : Debugging

     A reproducer is the smallest known program that induces incorrect behavior
     in the system. See :ref:`Make it fail` for more.

   Sharing

      Consider the following program:

      .. code-block:: haskell

         foo :: Int -> Int
         foo n = let x = [1..n]
                     in zip (fmap (* (last x)) x) x

      We say that ``x`` is *shared* in this program because each of the three
      references of ``x`` refer to the ``x`` defined in the ``let``. If ``x`` is
      not shared that the list ``[1..n]`` would be allocated *for each*
      reference of ``x``. Sharing is fundamental to performance oriented Haskell
      because it reduces allocations, leverages call-by-need, and saves work.

   Shotgun Debugging : Debugging

      Debugging with hope instead of process and measurement. See its Wikepedia
      `entry <https://en.wikipedia.org/wiki/Shotgun_debugging>`__.

   SRT : Runtime

      Static reference tables are how GHC's garbage collector determines the
      live :term:`CAF`'s of a program. SRTs are stored in a heap object's
      :term:`Info Table` and are simply an object in the compiled programs data
      segment. See `The SRT Note
      <https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Cmm/Info/Build.hs#L51>`__
      in ``GHC.Cmm.Info.Build`` for more details.

   Thunk

      A thunk is a special kind of :term:`Closure` that represents a suspended
      computation. Thunks reside on the heap and are the key feature that
      provides Haskell's laziness. See :cite:t:`SpinelessTaglessGMachine`
      Section 3.1.2 for more details.

   Thread State Object (TSO)

      A thread state object is a heap object that represents a Haskell thread in
      GHC's runtime system. For the precise contents please see its definition
      in :ghcSource:`GHC's source code
      <rts/include/rts/storage/TSO.h?ref_type=heads>` and `this description
      <https://youtu.be/5vKBFnTsCcE?si=4THBS_KMYRI6U1Sm&t=4620>`__ by Ben
      Gamari.

   Top-Level : Scope

      The most outer-most or global scope of the program.

   Unboxed : Levity

      An Unboxed value is a value that is represented by the value itself and
      not a pointer to an object on the heap. Unboxed values therefore cannot be
      lazy, like :term:`boxed` values.

   Unlifted : Levity

      An Unlifted type is a type where :math:`\bot` *is not* an element of that
      type. See :term:`Levity Polymorphism` and :term:`Lifted` types for more.

   Unknown function

      An unknown function is a function in the STG machine whose :term:`Entry
      Code` pointer and :term:`Arity` are not statically known by GHC. Unknown
      functions require GHC to generate code that first scrutinizes the function
      pointer to determine its arity and then dispatch to the normal function
      call handling procedures. This in known has a generic apply in the STG
      machine and is slower (due to needing to scrutinize the function) than a
      :term:`Known function`. See :cite:t:`fastCurry` for more details on STG
      calling conventions.

   Unfolding

      An Unfolding of an identifier, as defined in ``GHC.Core.Unfold``, is the
      *approximate* form the identifier would have if the identifier's
      definition was substituted for the identifier. That is, Unfoldings are
      generally the right hand sides or bodies of function definitions untouched
      by optimizations. Unfoldings appear in Core and Interface files to enable
      cross-module inlining and optimizations. See the :ref:`Reading Core
      <Reading Core>` chapter for more.


   WHNF : Normal Forms

      An expression is in *weak head normal form* if it has been evaluated to
      its' outermost data constructor or lambda abstraction (i.e., *the head*).
      See `this
      <https://stackoverflow.com/questions/6872898/what-is-weak-head-normal-form/6889335#6889335>`_
      post, `the wiki <https://wiki.haskell.org/Weak_head_normal_form>`_ , and
      `wikipedia
      <https://en.wikipedia.org/wiki/Lambda_calculus_definition#Weak_head_normal_form>`_
      for more.
