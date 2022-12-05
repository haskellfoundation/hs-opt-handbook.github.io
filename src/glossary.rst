.. _glossary:

Glossary
========

.. glossary::


   Boxed : Levity

      A Boxed value is a value that is represented by a pointer to the heap.

   Closure

      A closure is value that associates a function with an environment, where
      the environment maps every free variable in the function with a value or
      reference to which the free variable was bound when the closure was
      created. Closure's are the canonical way to realize lexical scoping in
      languages with first-class functions, such a Haskell. See `the wikipedia
      <https://en.wikipedia.org/wiki/Closure_(computer_programming)>`_ entry for
      more.

   CAF

     A CAF, or Constant Applicative Form, is a Haskell value which contains no
     free variables and is not a function. Consider these examples:

     .. code-block:: haskell

        -- these are CAFs
        foo :: Int
        foo = 12

        bar :: (Int, [Int])
        bar = ((*) 10 10, [1..])

        baz :: Int -> Int
        baz = (*) 3

        -- not CAFs
        qux :: Int -> Int
        qux e = e * 3     -- equivalent to baz but is a lambda so not a CAF

        quux :: Int -> Int
        quux = (*) 10 x   -- x is free thus

     These values are *constant* because they don't bind any variables or have
     any free variables. Because they are constant they are floated (see
     :term:`Let Floating`) to the top of the program, and statically allocated
     during compile time. Since they are statically allocated at compile time
     CAFs are pinned memory and special treatment in the garbage collector and
     the runtime system. Thus, heavily allocating CAFs can increase memory
     residency.

   DWARF : Format

      DWARF symbols are a widely used and standardized data format used to
      provide source level debugging. For more, see `the official webpage
      <https://dwarfstd.org/>`_

   Full Laziness transformation : Optimization

      A form of :term:`Let Floating` which moves let bindings out of lambda
      abstractions to avoid unnecessary allocation and computation. See
      :cite:t:`peytonjones1997a` Section 7.2.

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

      A Lifted type is a type that contains the value :math:`\bot`;
      which represents non-terminating computation. For example, the ``Bool``
      type is a set with three values: ``True``, ``False``, and :math:`\bot`.
      Therefore ``Bool`` is a Lifted type.

   Pinned : Memory

     Pinned memory is memory that is guaranteed to not be moved by GHC's garbage
     collector. This is most often useful for interfacing with foreign code.
     Note that pinned memory may lead to memory fragmentation and increased slop
     because it never moves. See `Well Typed's
     <https://well-typed.com/blog/2020/08/memory-fragmentation/>`_ post and the
     `wiki
     <https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/gc/pinned>`_
     for more.

   Thunk

      A thunk is a special kind of :term:`Closure` that represents a suspended
      computation. Thunks reside on the heap and are the key feature that
      provides Haskell's laziness. See :cite:t:`SpinelessTaglessGMachine`
      Section 3.1.2 for more details.

   Unboxed : Levity

      An UnBoxed value is a value that is represented by the value itself.
      UnBoxed values therefore cannot be lazy, like boxed values.

   Unlifted : Levity

      An Unlifted type is a type where :math:`\bot` *is not* an element of that
      type. See :term:`Levity Polymorphism` and :term:`Lifted` types for more.

   WHNF : Normal Forms

      An expression is in *weak head normal form* if it has been evaluated to
      its' outermost data constructor or lambda abstraction (i.e., *the head*).
      See `this
      <https://stackoverflow.com/questions/6872898/what-is-weak-head-normal-form/6889335#6889335>`_
      post, `the wiki <https://wiki.haskell.org/Weak_head_normal_form>`_ , and
      `wikipedia
      <https://en.wikipedia.org/wiki/Lambda_calculus_definition#Weak_head_normal_form>`_
      for more.
