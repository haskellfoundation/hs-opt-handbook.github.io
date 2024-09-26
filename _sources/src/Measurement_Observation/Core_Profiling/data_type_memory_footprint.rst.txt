.. _Memory Footprint of Data Types Chapter:

Memory Footprints of Data Types
===============================

Low level languages such as C or Rust use types to describe the memory
requirements of a symbol. In a performance oriented mindset this is a killer
feature and an essential tool to writing :ref:`non-pessimized
<Non-Pessimization>`, and :ref:`data-oriented <Data-Oriented Design Chapter>`
code.

Fortunately all is not lost. We can do similar reasoning in Haskell. This
chapter describes how to statically reason about the memory your data types will
require. After reading the chapter one should be able to read a data type
declaration and perform back of the napkin math to reason about the worst case
(no optimizations or sharing) memory footprint.

What is the Point
-----------------

Modern CPU's are fast and memory is abundant so what is the point. Certainly
these things are true, but for performance-oriented Haskell we care about the
caching behavior of our code. *Every piece* of code that one writes will go
through a CPU's L1 caches and the behavior of those caches will have an
immediate and drastic impact on the performance of your code. So much so that
not missing the data cache is one of the :ref:`golden rules <Golden Rules
Chapter>` of performance-oriented Haskell. The best way to have a cache hit
rather than a miss is to architect the program to require smaller data that can
more easily fit neatly into a cache line. Therefore, the first step is to
writing cache friendly code is understanding the memory footprint of your data
types so that you understand what you are asking of the CPU.

Atomic Types
------------

:term:`Atomic` types are defined and implemented in GHC as *builtins*. Most of
these types are a single machine word or two:

.. csv-table::
   :header: "Data Type", "Size (Words)", "Notes"

   "()",               0,    "will be shared"
   "Bool",             0,    "will be shared"
   "Char",             2,    "will be shared"
   "Int",              2,    "Could be shared, see the on sharing section"
   "Int8",              2,    "Due to alignment"
   "Int16",              2,    "Due to alignment"
   "Int32",              2,    ""
   "Int64",              2,    ""
   "Int64 (32-bit)",     3,    ""
   "Word",              2,    ""
   "Word8",              2,    "Due to alignment"
   "Word16",              2,    "Due to alignment"
   "Word32",              2,    ""
   "Word64",              2,    ""
   "Word64 (32-bit)",     3,    ""
   "Double",              2,    ""
   "Double (32-bit)",     3,    ""
   "Integer",              2,    ""
   "Integer (32-bit)",     3,    ""

Boxed Data Types
----------------

:term:`Boxed` data types are ubiquitous in Haskell programs. The strategy for
any boxed data type is to count the machine words required to represent each
data constructor. To count the machine words we count the number of fields, add
one for the constructor header (which is the constructor's :term:`info table`),
and then sum that amount to the amount of memory required for the data types
each field points to. This works because boxed types represent fields with
pointers, which are a single machine word. For example, consider this
monomorphic list-like data type:

.. code-block:: haskell

      data MyIntList = MyCons Int MyIntList
                     | Nil

This type has two constructors: ``Nil`` which has no fields, and ``MyCons``
which has two fields, an ``Int`` and the rest of the list. Therefore a single
``MyCons`` will need:

* one word for the ``MyCons`` constructor header.
* one word for a pointer to an ``Int`` plus the machine words needed to represent an ``Int``.
* one word for a pointer to a ``MyIntList`` plus the machine words required for
  the rest of the ``MyIntList``.


The only unknown is the memory footprint of an ``Int``. Fortunately, ``Int``'s
are boxed, so we can use the same strategy. Here is its definition:

.. code-block:: haskell

   -- in GHC.Types in the ghc-prim library
   -- ...
   -- | A fixed-precision integer type with at least the range @[-2^29 .. 2^29-1]@.
   -- The exact range for a given implementation can be determined by using
   -- 'Prelude.minBound' and 'Prelude.maxBound' from the 'Prelude.Bounded' class.
   data Int = I# Int#


The ``Int#`` is the payload, it is an :term:`atomic`, unboxed type. Thus an
``Int`` needs two words: one for the constructor header of ``I#``, and one for
the payload ``Int#``. This means a ``MyCons`` will require, in the worst case,
six machine words: 1 for ``MyCons``, 2 for the pointers, 2 for the ``Int`` and 1
for ``Nil`` because ``Nil`` is only a constructor.

As an example, consider a singleton list ``MyCons 7 Nil``. This what the
singleton will look like in memory:

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
    \node[data] (mycons) {MyCons};
    \node[pointer, right= -\the\pgflinewidth of mycons.east] (p1) {};
    \node[pointer, right= -\the\pgflinewidth of p1.east]     (p12) {};
    \node[data, below=of p1] (int) {I\#};
    \node[data, right= -\the\pgflinewidth of int.east] (seven) {7};
    \node[data, right=of p12] (nil) {Nil};

    % Pointers (arrows) between nodes
    \draw[pointer] (p1.center)  -- (int.north);
    \draw[dot]     (p1.center) circle (3pt);
    \draw[pointer] (p12.center) -- (nil.west);
    \draw[dot]     (p12.center) circle (3pt);

    \end{tikzpicture}

Each box is a machine word and each arrow is a pointer to some location in the heap.

Earlier I was careful to say *in the worst case* because our analysis does not
consider :term:`sharing` or strictness. In general, we cannot assess how much
sharing will happen at runtime without the code in question. However, we can
still make some safe assumptions. For example, in GHC there is only one empty
list which is repeatedly shared. If we assume that ``Nil`` has the same behavior
then ``MyCons 7 Nil`` will only require five words instead of six. For the
memory diagrams in this book, we'll represent sharing as a dashed outline when needed:

.. tikz::
    :libs: shapes, arrows.meta, positioning
    :align: center

    \begin{tikzpicture}[
        node distance=50pt and 1.5cm,
        data/.style={draw, minimum width=1.5cm, minimum height=2cm},
        shared/.style={draw, minimum width=1.5cm, minimum height=2cm, dashed},
        pointer/.style={draw, minimum width=1cm, minimum height=2cm,-{Stealth[scale=2]}},
        dot/.style={circle, fill, inner sep=1pt}
    ]

    % Nodes off the linked list
    \node[data] (mycons) {MyCons};
    \node[pointer, right= -\the\pgflinewidth of mycons.east] (p1) {};
    \node[pointer, right= -\the\pgflinewidth of p1.east]     (p12) {};
    \node[data, below=of p1] (int) {I\#};
    \node[data, right= -\the\pgflinewidth of int.east] (seven) {7};
    \node[shared, right=of p12] (nil) {Nil};

    % Pointers (arrows) between nodes
    \draw[pointer] (p1.center)  -- (int.north);
    \draw[dot]     (p1.center) circle (3pt);
    \draw[pointer] (p12.center) -- (nil.west);
    \draw[dot]     (p12.center) circle (3pt);

    \end{tikzpicture}

A More Complicated Example
^^^^^^^^^^^^^^^^^^^^^^^^^^

Our last example was simple, what about a type such as a ``Data.HashMap``:

.. code-block:: haskell

   -- from Data.HashMap.Internal
   data HashMap k v
       = Empty
       | BitmapIndexed !Bitmap !(A.Array (HashMap k v))
       | Leaf !Hash !(Leaf k v)
       | Full !(A.Array (HashMap k v))
       | Collision !Hash !(A.Array (Leaf k v))

This type has type variables ``k`` and ``a``, bang patterns and uses other types
such as: ``Bitmap``, ``Hash``, ``Leaf`` and ``Array``. For types like this the
strategy remains the same. We assess the memory footprint by counting machine
words for each constructor and each type in use. Type variables are represented
and counted as pointers. The only difference is that the memory footprint can
change depending an what the type variable reifies to. For example, a value of
``HashMap Bool v`` will have a smaller footprint than a value of ``HashMap
MyIntList v`` because ``Bool`` will have a smaller footprint than ``MyIntList``.
Furthermore, ``True`` and ``False`` are statically allocated data constructors
that are always shared, so the only memory costs incurred are pointers.

Note that our last example showed this without type variables via ``Nil``.
``Nil`` was the value that the ``MyIntList`` pointer in ``MyCons`` pointed to.
Imagine if the example had been ``MyCons 7 (MyCons 5 Nil)``, then this value
would have a larger footprint (seven words assuming a shared ``Nil`` ) because
the list tail pointer would have pointed to a heavier value than ``Nil``.

First let's assess the memory footprint for the type that are used in each
constructor:

.. code-block:: haskell

   -- all of these are from Data.HashMap.Internal
   type Bitmap = Word
   ...
   type Hash   = Word
   ...
   data Leaf k v = L !k v

We see that ``Bitmap`` and ``Hash`` are a single machine word and ``Leaf`` is a
partially strict pair with two fields. ``Leaf`` will need a total of three words
plus the size of ``k`` and ``v``. For now, we will ignore strictness and focus
only on the worst case, we'll return to strictness later in the chapter.


This only leaves ``Array``. Here is its definition:

.. code-block:: haskell

   -- Data.HashMap.Internal.Array
   data Array a = Array {unArray :: !(SmallArray# a)}

An ``Array`` is a wrapper around an unboxed ``SmallArray#``. ``SmallArray#`` is
one of GHC's primitive types that are exposed through the ``GHC.Exts`` module in
``base``, but are defined in the `ghc-prim
<https://hackage.haskell.org/package/ghc-prim-0.11.0/docs/GHC-Prim.html#g:19>`__
boot library for GHC:

.. code-block:: C

   // in rts/storage/Closures.h
   // A small array of head objects, ie SmallArray# and MutableSmallArray#
   //
   // Closure types: SMALL_MUT_ARR_PTRS_CLEAN, SMALL_MUT_ARR_PTRS_DIRTY,
   // SMALL_MUT_ARR_PTRS_FROZEN_DIRTY, SMALL_MUT_ARR_PTRS_FROZEN_CLEAN,
   typedef struct {
       StgHeader   header;
       StgWord     ptrs;
       StgClosure *payload[] MUT_FIELD;
   } StgSmallMutArrPtrs;

.. note::

   You'll notice that I do not discuss ``MUT_FIELD``, this macro is used by
   GHC's RTS to mark fields which can be modified by the mutator during garbage
   collection, but it expands to nothing. For more see this `note
   <https://gitlab.haskell.org/ghc/ghc/-/blob/master/rts/include/stg/SMP.h?ref_type=heads#L144>`__.

A ``smallArray#`` is a C struct with a ``StgHeader``, a ``StgWord`` and an array
of pointers that point to the closures which are the contents of the array. An
``StgHeader`` is defined as:

.. code-block:: C

   typedef struct {
    // If TABLES_NEXT_TO_CODE is defined, then `info` is offset by
    // `\texttt{sizeof}(StgInfoTable)` and so points to the `code` field of the
    // StgInfoTable! You may want to use `get_itbl` to get the pointer to the
    // start of the info table. See
    // https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects#tables_next_to_code.
    const StgInfoTable* info;

   #if defined(PROFILING)
       StgProfHeader         prof;
   #endif
   } StgHeader;

We'll assume we are not profiling and are compiling with tables-next-to-code.
With these assumptions the ``StgHeader`` is just a pointer to a ``StgInfoTable``
and thus just a single machine word. Similarly, an ``StgWord`` is a
machine word:

.. code-block:: c

   // in ghc/rts/includes/stg/Types.h
   /*
    * Stg{Int,Word} are defined such that they have the exact same size as a
    * void pointer.
    */
   #if SIZEOF_VOID_P == 8
   typedef int64_t            StgInt;
   typedef uint64_t           StgWord;
   ...
   #elif SIZEOF_VOID_P == 4
   typedef int32_t            StgInt;
   typedef uint32_t           StgWord;
   ...
   #endif

This means that a singleton array will be one word for the header, one word for
the ``ptrs`` field, ``n`` pointers for ``n`` elements plus the size of those
elements. We can summarize this all into a equation that calculates the size of
``n`` element array: :math:`2 + n + n(\texttt{sizeof}(element))`.


Let's walk through how we derived that equation. We know that there will always
be a constant overhead of two words due to the header and ``ptrs`` field, and
that each element will require a pointer. This gives us ``2 + n``; then all that
is left is the size of the elements themselves and which we know we have ``n``
of. Let's use a datatype as an example to check our assumptions. Assume we have
a *singleton* array consisting of an ``Int``. This singleton array will need one
word for the header, one for the ``ptrs`` field; this is the constant 2. Next it
will need one pointer to the ``Int`` heap object because :math:`n = 1`, and
finally two words for the ``Int`` itself. Thus, yielding a total of five words.
Here is the calculation:

.. math::
   :nowrap:

   \begin{align*}
        \texttt{sizeof}(\text{SmallArray}, 1, \text{Int}) =&\; 2 + 1 + 1(\texttt{sizeof}(\text{Int})) \\
        =&\; 3 + 1(\texttt{sizeof}(\text{Int})) \\
        =&\; 3 + 1(2) \\
        =&\; 3 + 2 \\
        =&\; 5     \\
   \end{align*}

Now consider an array with two ``Int``'s. This array will require three *more*
words: one more pointer, and two more for the ``Int`` payload, for a total of
eight:

.. math::
   :nowrap:

   \begin{align*}
        \texttt{sizeof}(\text{SmallArray}, 2, \text{Int}) =&\; 2 + 2 + 2(\texttt{sizeof}(\text{Int})) \\
        =&\; 4 + 2(\texttt{sizeof}(\text{Int})) \\
        =&\; 4 + 2(2) \\
        =&\; 4 + 4 \\
        =&\; 8     \\
   \end{align*}

Let's take stock of what we know:

* ``Bitmap`` :math:`\rightarrow`  1 word
* ``Hash``     :math:`\rightarrow`  1 word
* ``Leaf``     :math:`\rightarrow`  :math:`3 + \texttt{sizeof}(k) + \texttt{sizeof}(v)`  words
* ``Array``   :math:`\rightarrow`  :math:`2 + n + n(\texttt{sizeof}(v))`  words (``v``
  becomes the array elements by definition)

Now we can assess ``HashMap``. ``HashMap`` has five constructors. We'll proceed
in order beginning with ``Empty``. ``Empty`` will take a single word just like
``Nil``. ``BitmapIndexed`` is next. It is defined as:

.. code-block:: haskell

   BitmapIndexed !(Bitmap) !(A.Array (HashMap k v))

So we have one word for the constructor, one for the ``Bitmap`` and then the
array. The best we can do is express the size in terms of ``k`` and ``v`` since
we do not know what these types will reify to. This gives us:

.. math::
   :nowrap:

   \begin{align*}
        \texttt{sizeof}(\text{BitmapIndexed}, n, k, v) =&\; 1 + 1 +\texttt{sizeof}(\text{Array}(\text{HashMap}\;k\;v)) \\
        =&\; 2 +\texttt{sizeof}(\text{Array}(\text{HashMap}\;k\;v)) \\
        =&\; 2 +(2 + n + n(\texttt{sizeof}(\text{HashMap}\;k\;v))       \\
        =&\; 4 + n + n(\texttt{sizeof}(\text{HashMap}\;k\;v)
   \end{align*}

Following ``BitmapIndexed`` is ``Leaf``. ``Leaf`` is defined as:

.. code-block:: haskell

   Leaf !Hash !(Leaf k v)

We have one word for the ``Leaf`` constructor, two words for ``Hash``: one for a
pointer and one for the payload we deduced above. ``Hash`` is not unpacked and
thus is represented as a pointer to the ``Hash`` payload. This leaves only the
``Leaf`` type which we analyzed earlier. For definitions like this, it is
helpful to inline the constituent data types, like so:

.. code-block:: haskell

   Leaf !Hash !(L !k v)

This flattens the data structure and keeps everything in one place. The only
caveat is that one must remember there is a pointer that points to ``L`` from
``Leaf``. I've indicated this with the open parenthesis: ``(`` . Recall that
``L`` defines a strict pair which requires three words, one for ``L`` and two
pointers to each element. Now we can calculate the memory footprint in terms of
``k`` and ``v`` just as before:

.. math::
   :nowrap:

   \begin{align*}
        \texttt{sizeof}(\text{Leaf}, k, v) =&\; 1 + 2 +(1 +
        \texttt{sizeof}(\text{Leaf k v})) \\
        =&\; 4 + \texttt{sizeof}(\text{Leaf k v})) \\
        =&\; 4 + (3 + \texttt{sizeof}(k) + \texttt{sizeof}(v)) \\
        =&\; 7 + \texttt{sizeof}(k) + \texttt{sizeof}(v))
   \end{align*}


All we have left is ``Full`` and ``Collision``. ``Full`` is a special case. The
unordered-containers HashMap is a Hashed Mapped Array Trie :cite:p:`BagwellHAMT`
where a full array contains a maximum of 32 elements. Thus a ``Full`` will need
one word for the ``Full`` header, and then
:math:`\texttt{sizeof}(\text{Array}(\text{HashMap}\;k\;v))`. Fortunately we
already calculated that for ``BitmapmIndexed``. Thus a ``Full`` will be:

.. math::
   :nowrap:

   \begin{align*}
        \texttt{sizeof}(\text{Full}, 32, k, v) =&\; 1 +\texttt{sizeof}(\text{Array}(\text{HashMap}\;k\;v)) \\
        =&\; 1 + (2 + n + n(\texttt{sizeof}(\text{HashMap}\;k\;v))       \\
        =&\; 1 + (2 + 32 + 32(\texttt{sizeof}(\text{HashMap}\;k\;v))       \\
        =&\; 35 + 32(\texttt{sizeof}(\text{HashMap}\;k\;v)
   \end{align*}


All we have left is ``Collision``. Fortunately we've already calculated the
footprint of ``Collision`` because ``Collision`` is defined as:

.. code-block:: haskell

   Collision !Hash !(A.Array (Leaf k v))

which is isomorphic to ``BitmapIndexed`` because ``Hash`` and ``Bitmap`` have
the same footprint and we know how to calculate ``A.Array (Leaf k v)``. Thus:

.. math::
   :nowrap:

   \begin{align*}
        \texttt{sizeof}(\text{Collision}, n, k, v) =&\; 1 + 1 +\texttt{sizeof}(\text{Array}(\text{HashMap}\;k\;v)) \\
        =&\; 4 + n + n(\texttt{sizeof}(\text{HashMap}\;k\;v)
   \end{align*}

As an aside, this result reveals potential performance issues on reads for a
full hash map. 35 words is 280 bytes, a cache line is typically 64 bytes (or 8
words) so just with the constants, a ``Full`` will consume 4 cache lines
_unevenly_; :math:`64 * 4 = 256` always leaving 24 bytes leftover on the fifth
cache line. Furthermore let's consider what the memory footprint of a
``HashMap Int Int`` that is a single ``Full`` will be. This will require:

.. math::

    \begin{align*}
            \texttt{sizeof}(\texttt{Full}, 32, \texttt{Int}, \texttt{Int}) =&\; 35 +
            32(\texttt{sizeof}(\text{HashMap}\;\texttt{Int}\;\texttt{Int}) \\
            =&\; 35 + 32(\texttt{sizeof}(\text{Leaf}\;\texttt{Int}\;\texttt{Int}) \\
            =&\; 35 + 32(7 + \texttt{sizeof}(\texttt{Int}) + \texttt{sizeof}(\texttt{Int})) \\
            =&\; 35 + 32(7 + 2 + 2) \\
            =&\; 35 + 32(11) \\
            =&\; 35 + 352    \\
            =&\; 387
    \end{align*}

A total of 387 words which is 3, 096 bytes or roughly 3KiB, just for 32
elements! Unfortunately, this also will not be a cache friendly data structure.
To store 387 words we need :math:`\frac{387}{8} = 48` cache lines. But 387 is
not a multiple of 2, so there will be :math:`387 \bmod{} 8 = 3` words of
leftover space on the final cache line [#]_. A more cache friendly data
structure would ensure that a ``Full`` always fit evenly into a set of cache
lines and would thereby avoid fragmenting the cache. One caveat is that this
wasted space will change depending on the sizes of the key and value.

Other Ways
----------

We've learned how to statically analyze a data type. But often times there are
easier ways than reasoning. For example, you can always :ref:`weigh <Weigh
Chapter>` your data types. Although this *will* consider sharing.

Read the Assembly Output
^^^^^^^^^^^^^^^^^^^^^^^^

Besides ``weigh``, we can practice :ref:`don't think look <Don't Think, Look>`
by inspecting the ``.data`` sections of GHC's generated assembly. Here is an
example for some primitive types. The idea is that if we create a top level
binding that will be *statically* allocated at compile time. This static
allocation will then appear in the ``.data`` section of the assembly code. Here
is a quick refresher on assembly sizes:

.. csv-table::
   :header: "Directive", "Size (Bytes)"

   ".byte",             1
   ".word",             2
   ".long",             4
   ".quad",             8

And here is our example program.

.. code-block:: haskell

   {-# OPTIONS_GHC -O2 -ddump-asm #-}

   {-# NOINLINE a_unit #-}
   a_unit :: ()
   a_unit =  ()

   {-# NOINLINE x #-}
   a_bool :: Bool
   a_bool = True

   {-# NOINLINE a_int8 #-}
   a_int8 :: Int8
   a_int8 = 1

   {-# NOINLINE a_int16 #-}
   a_int16 :: Int16
   a_int16 = 1823

   {-# NOINLINE a_int #-}
   a_int :: Int
   a_int = 123

   {-# NOINLINE a_int64 #-}
   a_int64 :: Int64
   a_int64 = 1234

   {-# NOINLINE a_word8 #-}
   a_word8 :: Word8
   a_word8 = 1

   {-# NOINLINE a_word16 #-}
   a_word16 :: Word16
   a_word16 = 12

   {-# NOINLINE a_word #-}
   a_word :: Word
   a_word = 123

   {-# NOINLINE a_word64 #-}
   a_word64 :: Word64
   a_word64 = 1234

We'll handle these in chunks beginning with a unit. Recall that in the
:ref:`Weigh Chapter <Weigh Chapter>` a ``()`` was measured to be 0 allocations
because only one ``()`` exists in GHC and is shared for all references. But here
we can see the truth:

.. code-block:: asm

   .section .data
   .align 8
   .align 1
   .globl Main.a_unit_closure
   .type Main.a_unit_closure, @object
   Main.a_unit_closure:
      .quad	()_con_info

Our unit value is a reference to the shared info-table for the one true ``()``,
which requires a ``.quad``. ``.quad`` is a *quad word* meaning it is 8 bytes
(quad because its four ``.word``'s), or 64-bits (this was run on a 64-bit
machine). What about ``Bool``:

.. code-block:: asm

   .section .data
   .align 8
   .align 1
   .globl Main.a_bool_closure
   .type Main.a_bool_closure, @object
   Main.a_bool_closure:
   	.quad	GHC.Types.True_con_info

The same is true for ``a_bool``; the ``True`` is a pointer to the one ``True``
value called ``GHC.Types.True_con_info``, and thus takes a single word. Let's
check the Int-like fixnums:

.. code-block:: asm

   .section .data
   .align 8
   .align 1
   .globl Main.a_int8_closure
   .type Main.a_int8_closure, @object
   Main.a_int8_closure:
   	.quad	GHC.Int.I8#_con_info
   	.byte	8
   	.long	0
   	.word	0
   	.byte	0

   ...

   .section .data
   .align 8
   .align 1
   .globl Main.a_int16_closure
   .type Main.a_int16_closure, @object
   Main.a_int16_closure:
   	.quad	GHC.Int.I16#_con_info
   	.word	16
   	.long	0
   	.word	0

   ...

   .section .data
   .align 8
   .align 1
   .globl Main.a_int_closure
   .type Main.a_int_closure, @object
   Main.a_int_closure:
   	.quad	GHC.Types.I#_con_info
   	.quad	32

   ...

   .section .data
   .align 8
   .align 1
   .globl Main.a_int64_closure
   .type Main.a_int64_closure, @object
   Main.a_int64_closure:
   	.quad	GHC.Int.I64#_con_info
   	.quad	64

We see that our ``a_int8 :: Int8`` requires one word for the data constructor
header: ``.quad GHC.Int.I8#_con_info``, and then requires one byte for the
payload: ``.byte 8``, but then requires another 7 bytes: ``.long 0``, ``.word
0``, ``.byte 0`` all initialized to 0. GHC is smartly padding this value to fit
evenly into the data cache. Without this padding, ``a_int`` would require *65*
bytes (the info table pointer + 1 byte for the payload) which is guaranteed to
never fit evenly into the data cache. This padding is good, we should be happy
that GHC does it for us. However, the padding is still empty space which is
wasteful. If this were a real program a good optimization would be to increase
memory efficiency by utilizing this extra space. See the :ref:`Data-Oriented
Design Case Study <Data Oriented Design Case Study>` for an example.

We see similar padding for ``a_int16``, while ``a_int`` and ``a_int64`` take two
words as we expect. The sized ``Word`` types are identically sized to the
``Int`` types:

.. code-block:: asm

   .section .data
   .align 8
   .align 1
   .globl Main.a_word8_closure
   .type Main.a_word8_closure, @object
   Main.a_word8_closure:
   	.quad	GHC.Word.W8#_con_info
   	.byte	8
   	.long	0
   	.word	0
   	.byte	0

   ...

   .section .data
   .align 8
   .align 1
   .globl Main.a_word16_closure
   .type Main.a_word16_closure, @object
   Main.a_word16_closure:
   	.quad	GHC.Word.W16#_con_info
   	.word	16
   	.long	0
   	.word	0

   ...

   .section .data
   .align 8
   .align 1
   .globl Main.a_word_closure
   .type Main.a_word_closure, @object
   Main.a_word_closure:
   	.quad	GHC.Types.W#_con_info
   	.quad	32

   ...

   .section .data
   .align 8
   .align 1
   .globl Main.a_word64_closure
   .type Main.a_word64_closure, @object
   Main.a_word64_closure:
   	.quad	GHC.Word.W64#_con_info
   	.quad	64

Unfortunately, this will only work for boxed data types as we cannot (`yet
<https://gitlab.haskell.org/ghc/ghc/-/issues/17521>`__ have a top level unlifted
data type (which an unboxed type is). Another curiosity are the representation
of top level :term:`compound types`. For example, with this:

.. code-block:: haskell

   {-# NOINLINE a_pair#-}
   a_pair :: (Int,Int)
   a_pair =  (1,2)

   {-# NOINLINE a_list#-}
   a_list :: [Int16]
   a_list =  [1,2,3,4]

GHC generates:

.. code-block:: text

   .section .data
   .align 8
   .align 1
   .globl Main.a_pair_closure
   .type Main.a_pair_closure, @object
   Main.a_pair_closure:
	   .quad	(,)_con_info
	   .quad	stg_INTLIKE_closure+273
	   .quad	stg_INTLIKE_closure+289
	   .quad	3

.. admonition:: Help Wanted
   :class: help-wanted

   These ``+`` disturb the ``asm`` lexer in sphinx. I've tried to escape them to
   no avail so this block lacks syntax highlighting. If you know how to resolve
   this please `contribute
   <https://github.com/haskellfoundation/hs-opt-handbook.github.io/issues/117>`__!

Which is just what we expected: one word (``.quad (,)_con_info``) for the data
constructor header, one for ``fst`` (``.quad stg_INTLIKE_closure+273`` ), and
one ``snd`` (``.quad stg_INTLIKE_closure+289``). However, GHC has added another
word that is mysteriously set to 3: ``.quad 3``. This extra word is an
optimization that GHC applies which tags the symbol ``Main.a_pair_closure`` as a
static constructor that contains no :term:`CAF` references. This tag (the 3)
instructs the garbage collector to ignore this symbol during garbage collection.
If ``Main.a_pair_closure`` was found to possibly have a CAF then the tag would
have been 0 but the extra word would still exist [#]_ . So does this mean that
our analysis is incorrect? No, this data is only checked and loaded during a
garbage collection event, it is a by product of our abuse of ``NOINLINE`` to
create a static top-level closure.

About Strictness
^^^^^^^^^^^^^^^^

Now we can finally discuss strictness. Consider this program:

.. code-block:: haskell

   module Example where

   import Data.Word

   data Foo = Foo Word16 Word16 Word16 Word16

   {-# NOINLINE a_foo #-}
   a_foo :: Foo
   a_foo = Foo 123 234 345 456

   main :: IO ()
   main = return ()

``Foo`` has one constructor and four fields; each of which is a ``Word16``. So
we should expect that a ``Foo`` will be nine words: one for the header, and then
each ``Word16`` is a pointer to the ``Word16#`` payload which will include
padding to align to 64 bits. Let's check the assembly:

.. code-block:: text

   Example_a_foo_closure:
       .quad   Example_Foo_con_info
       .quad   Example_a_foo4_closure+1  ;; these closures are the "boxes"
       .quad   Example_a_foo3_closure+1  ;; in boxed data types!
       .quad   Example_a_foo2_closure+1  ;; this +1 is a pointer tag
       .quad   Example_a_foo1_closure+1  ;; meaning the value is evaluated.
       .quad   3

Ignoring the ``.quad 3`` tag, we find a word for the constructor's info table:
``.quad Example_Foo_con_info``, and a word for each field which is the
aforementioned pointers. Here are what those closures look like:

.. code-block:: asm

   Example_a_foo4_closure:
        .quad   base_GHCziWord_W16zh_con_info
        .word   123
        .long   0
        .word   0
   Example_a_foo3_closure:
           .quad   base_GHCziWord_W16zh_con_info
           .word   234
           .long   0
           .word   0
   Example_a_foo2_closure:
           .quad   base_GHCziWord_W16zh_con_info
           .word   345
           .long   0
           .word   0
   Example_a_foo1_closure:
           .quad   base_GHCziWord_W16zh_con_info
           .word   456
           .long   0
           .word   0

Each closure is two words: one for the ``Word16`` info table,
``.quad base_GHCziWord_W16zh_con_info``;
then two bytes for the payloads, ``.word 123``,
``.word 234`` etc.; and then six bytes of padding, ``.long 0`` and ``.word 0``.
This matches our expectations. Now let's add some strictness to the first two fields:

.. code-block:: haskell

   module Example where

   import Data.Word

   -- now the first two fields are strict
   data Foo = Foo !Word16 !Word16 Word16 Word16

   {-# NOINLINE a_foo #-}
   a_foo :: Foo
   a_foo = Foo 123 234 345 456

   main :: IO ()
   main = return ()

and here is the assembly:

.. code-block:: text

   Example_a_foo_closure:
           .quad   Example_Foo_con_info
           .quad   Example_a_foo2_closure+1
           .quad   Example_a_foo1_closure+1
           .word   123
           .word   234
           .long   0
           .quad   3

Our ``a_foo`` object has reduced in size! The payloads for first two fields are
now inlined into the closure. Now ``a_foo`` requires one word for the header,
four for the third and fourth fields, and then just one extra word for padding
and the first and second fields yielding a total footprint of 6 words. Let's
make ``Foo`` strict in every field, taking this process to its logical
conclusion:

.. code-block:: haskell

   {-# LANGUAGE StrictData #-}
   module Example where

   import Data.Word

   -- Now we use Strict data instead of bang patters
   data Foo = Foo Word16 Word16 Word16 Word16

   -- also notice that NOINLINE is not necessary
   a_foo :: Foo
   a_foo = Foo 123 234 345 456

   main :: IO ()
   main = return ()

which generates the following assembly:

.. code-block:: asm

   Example_a_foo_closure:
        .quad   Example_Foo_con_info
        .word   123
        .word   234
        .word   345
        .word   456

Exactly what we expected. ``a_foo`` is now only two words. Good job GHC! What is
happening here is that GHC's :ref:`Demand Analysis <Demand Analysis Chapter>`
has concluded that it is safe to unbox the ``Word16`` payloads for ``a_foo``
because they are marked as strict. The key point is that strictness can matter
when assessing memory footprints. For the worst case, you should assume demand
analysis cannot help you and analyze your data structures as we did above.
Assuming GHC's demand analysis works in your favor gives you the best case. Be
sure to :ref:`don't think look <Don't Think, Look>` by checking the Cmm or the
assembly. Here is what the Cmm in this case will looks like:

.. code-block:: haskell

   [section ""data" . a_foo_closure" {
        a_foo_closure:
            const Foo_con_info;
            const 123 :: W16;
            const 234 :: W16;
            const 345 :: W16;
            const 456 :: W16;
    }]

Which also shows the more compact ``foo`` closure.

About Sharing
^^^^^^^^^^^^^

Consider this program:

.. code-block:: haskell

   module Example where

   {-# NOINLINE a_list #-}
   a_list :: [Int]
   a_list = 1729 : []

   main :: IO ()
   main = return ()

This program defines a singleton list just like ``MyIntList`` above. Earlier we
stated that ``Nil``, or ``[]`` is shared. Now we can read the assembly to
observe what exactly that looks like. If it is actually shared then we should
see some kind of reference to the builtin ``[]`` constructor. Here is the
relevant assembly:

.. code-block:: text

   Example_a_list_closure:
           .quad   ghczmprim_GHCziTypes_ZC_con_info
           .quad   .LrH2_closure+1
           .quad   ghczmprim_GHCziTypes_ZMZN_closure+1
           .quad   3

.. code-block:: asm

   .LrH2_closure:
     .quad   ghczmprim_GHCziTypes_Izh_con_info
     .quad   1729

Sure enough, we see an info table pointer:
``.quad ghczmprim_GHCziTypes_ZC_con_info`` for ``:``; a pointer to our payload,
``.quad .LrH2_closure+1``; and a pointer to some other closure,
``.quad ghczmprim_GHCziTypes_ZMZN_closure+1``. Notice that both of the ghc-prim pointers
are `Z-encoded
<https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/ghc-boot/GHC/Utils/Encoding.hs?ref_type=heads#L43>`__. So we'll `translate
<https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/ghc-boot/GHC/Utils/Encoding.hs?ref_type=heads#L119>`__:

.. code-block:: haskell

   -- in ghc-boot library
   -- GHC.Utils.Encoding module

   -- Constructors
   ...
   encode_ch '['  = "ZM"
   encode_ch ']'  = "ZN"
   encode_ch ':'  = "ZC"
   ...

   decode_lower 'i' = '.'
   ...
   decode_lower 'm' = '-'


So a ``ghczmprim_GHCziTypes_ZC_con_info`` is ``ghc-prim_GHC.Types_:_con_info``,
and a ``ghczmprim_GHCziTypes_ZMZN_closure+1`` is
``ghc-prim_GHC.Types_[]_closure+1``! This is what sharing looks like at the
assembly level. That is, sharing is simply a reference to the shared data type.
Note that this is more easily observed in the Cmm:

.. code-block:: haskell

   [section ""data" . a_list_closure" {
     a_list_closure:
         const :_con_info;
         const a_list1_rHe_closure+1;
         const []_closure+1;
         const 3;
   }]

   [section ""data" . a_list1_rH2_closure" {
       a_list1_rH2_closure:
           const I#_con_info;
           const 1729;
   }]


Because Cmm is not z-encoded yet.

I say *usually* on purpose; consider this program:

.. code-block:: haskell

   module Example where

   {-# NOINLINE a_list #-}
   a_list :: [Int]
   a_list = 255 : []

   main :: IO ()
   main = return ()

The only change is that the value is now 255 rather than `ramanujan's number
<https://en.wikipedia.org/wiki/1729_(number)>`__. Now let's look at
the corresponding assembly:

.. code-block:: text

   Example_a_list_closure:
        .quad   ghczmprim_GHCziTypes_ZC_con_info
        .quad   stg_INTLIKE_closure+4337
        .quad   ghczmprim_GHCziTypes_ZMZN_closure+1
        .quad   3

Our closure has changed! Instead of a pointer to a closure that is observable we
have an offset pointer: ``stg_INTLIKE_closure+4337``. We are observing a nuance
in GHC's runtime system. GHC stores `static representations
<https://gitlab.haskell.org/ghc/ghc/-/blob/master/rts/StgMiscClosures.cmm?ref_type=heads#L991>`__
to ``Char`` and small ``Int``'s so that it can cleverly replace these heap
objects with static references. 255 is the largest ``Int`` of this kind that
will be shared, had we chosen 256, then GHC would produce:

.. code-block:: text

   Example_a_list_closure:
        .quad   ghczmprim_GHCziTypes_ZC_con_info
        .quad   .LrH2_closure+1
        .quad   ghczmprim_GHCziTypes_ZMZN_closure+1
        .quad   3
  .LrH2_closure:
        .quad   ghczmprim_GHCziTypes_Izh_con_info
        .quad   256

Which is identical to what we observed earlier. Note that this only occurs with
an ``Int``. GHC will not generate this code with an ``Int8``, ``Word8``,
``Int64`` and the rest.

Use GHC-vis
^^^^^^^^^^^

Another method is to use `ghc-vis <https://dennis.felsing.org/ghc-vis/>`__ .
GHC-vis is a plugin for GHCi that outputs graphiz graphs which show the memory
representation of a data type. Its documentation is very readable and it is
still actively maintained so we encourage interested parties to contribute or
give it a try. One caveat though: the documentation is representative of GHC's
internal types pre-GHC-9.0.

Summary
=======

We've come a long way. We have added two tools to our optimization toolbox.
First, a method to observe the memory footprint of a data type by inspecting the
GHC generated assembly. Second, a method to analyze the worst case size of a
data type by reading its definition and assessing its footprint. Along the way
we have also observed the effect of strictness, what sharing looks like at a low
level and observed GHC character and small ``Int`` cache pool.

Assessing the memory footprint of your data types should be one of the first
techniques you employ. It is non-invasive, doesn't require a full rebuild, and
will help you understand what the CPU must do in order to compute your program.
More importantly it is requisite to understanding the cache behavior of your
program. This can be especially powerful if you already know where the hot loop
in your implementation *or* architecture is. By reducing the memory footprint of
that data you'll be all but guaranteed to speed up your system.

.. note::

   If this chapter has helped you implement a speedup in your system or if you
   want to add a footprint derivation just as I did for ``HashMap`` above then
   please `contribute <https://github.com/haskellfoundation/hs-opt-handbook.github.io/blob/main/Contributing.rst>`__!


References
==========

.. [#] The HashMap behaves this way because of `yours truly
       <https://github.com/haskell-unordered-containers/unordered-containers/pull/317>`__.
       Even though the cache behavior is poor, the 16-bit base was worse because
       it created HashMaps with a more deeply nested structure. This meant even
       *more* pointer chasing in full cases. For the interested, you can observe
       the effect in the data posted in the pull request I have linked.

.. [#] The `Haskell Wiki
       <https://www.fpcomplete.com/blog/2016/05/weigh-package/>`_ page. Although
       it has not been updated in some time.

.. [#] For the interested, `here
   <https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/StgToCmm/Heap.hs?ref_type=heads#L215>`__
   is where the tag is applied and `here
   <https://gitlab.haskell.org/ghc/ghc/-/blob/master/rts/sm/Storage.h?ref_type=heads#L134>`__
   is an explanation of the tag from the perspective of the garbage collector.
   This tag will also appear in Cmm because it is applied during the STG to Cmm
   pass. For example here is the Cmm data declaration for ``a_pair`` :

   .. code-block:: haskell

      [section ""data" . Main.a_pair_closure" {
           Main.a_pair_closure:
               const (,)_con_info;
               const stg_INTLIKE_closure+273;
               const stg_INTLIKE_closure+289;
               const 3;
       }]

   Which indeed shows the ``const 3``.
