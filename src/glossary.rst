.. _glossary:

Glossary
========

.. glossary::

   WHNF : Normal Forms

      An expression is in *weak head normal form* if it has been evaluated to
      its' outermost data constructor or lambda abstraction (i.e., *the head*).
      See `this
      <https://stackoverflow.com/questions/6872898/what-is-weak-head-normal-form/6889335#6889335>`_
      post, `the wiki <https://wiki.haskell.org/Weak_head_normal_form>`_ , and
      `wikipedia
      <https://en.wikipedia.org/wiki/Lambda_calculus_definition#Weak_head_normal_form>`_
      for more.

   Boxed : Levity
      A Boxed value is a value that is represented by a pointer to the heap.

   Unboxed : Levity
      An UnBoxed value is a value that is represented by the value itself.
      UnBoxed values therefore cannot be lazy, like boxed values.

   Lifted : Levity
      A Lifted type is a type that contains the value :math:`\bot`;
      which represents non-terminating computation. For example, the ``Bool``
      type is a set with three values: ``True``, ``False``, and :math:`\bot`.
      Therefore ``Bool`` is a Lifted type.

   Unlifted : Levity
      An Unlifted type is a type where :math:`\bot` *is not* an element of that type.

   Thunk
      A thunk is a special kind of :term:`Closure` that represents a suspended
      computation. Thunks reside on the heap and are the key feature that
      provides Haskell's laziness. See `The Spineless-Tagless G-Machine
      <https://www.microsoft.com/en-us/research/wp-content/uploads/1992/04/spineless-tagless-gmachine.pdf>`_
      Section 3.1.2 for more details.

   Closure
      A closure is value that associates a function with an environment, where
      the environment maps every free variable in the function with a value or
      reference to which the free variable was bound when the closure was
      created. Closure's are the canonical way to realize lexical scoping in
      languages with first-class functions, such a Haskell. See `the wikipedia
      <https://en.wikipedia.org/wiki/Closure_(computer_programming)>`_ entry for
      more.
