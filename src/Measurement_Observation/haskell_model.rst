
Haskell Compilation and Execution Model
=======================================


GHC doesn't use a VM but compiles to native code (when a native code generator
is used).
So we can use tools to profile native code execution: e.g. perf.
However the native code we execute doesn't look exactly like the one produced by
imperative languages (C, C++, Rust, etc.) and this confuses the tools which make
some assumptions.
For example:

1. we don't have clear procedure boundaries with call/return instruction pairs
   (we use tail calls, i.e. jump instructions)

2. we don't use the so-called C stack in the usual way. I.e. we don't use usual
   stack registers (e.g. rsp/rbp on x86-64) as stack registers.

3. GHC uses its own calling convention

In addition, lazy evaluation makes control-flow and memory usage difficult to
understand.

In this chapter we describe the compilation pipeline and the execution model.
For each stage of the pipeline we list what can be measured at runtime.


Compilation pipeline overview
-----------------------------

GHC compiles Haskell codes in several stages, each with their own intermediate
representation:

Haskell: the functional language we love with its bazillion of extensions.

Core: a simple and explicitly typed functional language. Types are basically
Haskell types. Type-class dictionaries are passed explicitly as records,
type-applications are used everywhere needed, coercions (proofs that we can
convert a type into another) are first class values, etc.

STG: a functional language closer to the execution model. It's still typed but
with primitive types. E.g. it tracks if a value is a heap object or an
unboxed word (unboxed = doesn't have its own heap object) but not the Haskell
type of the heap object. Complex forms are lowered to simpler ones (e.g. unboxed
sums are lowered into unboxed tuples); this is called unarisation.

Additionally every heap allocation is made explicit with a let-binding. For
example instead of ``foo (MkBar x y)`` you have ``let b = MkBar x y in foo b``.
This is called `A-normal form<https://en.wikipedia.org/wiki/A-normal_form>`_.
During code generation from STG we know that all functions are applied to
"simple" arguments only (variables, constants, etc.).

Following the approach pioneered with "super-combinators", every top-level STG
binding is then compiled into imperative code. The idea is that executing this
imperative code has the same result as interpreting the functional code. For
example, ``let b = MkBar x y in foo b`` is basically compiled to:

.. code-block:: C

  b <- allocate heap object for `MkBar x y`
  evaluate foo
  apply `foo` to `b`

Different GHC backends use different imperative representations. The interpreter
uses ByteCode, the JavaScript backend uses some JavaScript-like form, all other
backends use Cmm.

Cmm: an imperative language that looks like LLVM IR. It supports expressions,
statements, and it abstracts over machine primops, machine registers, stack
usage, and calling conventions. A pass performs register assignment and stack
layout for the target architecture. Then assembly code (textual) is generated
for the target architecture and an external assembler program generates machine
code (binary) for it. Finally an external linker is used to transform the
resulting code objects into a single executable or library.

Executables are linked against a runtime system (RTS) that provides primitives
to manage:
- memory: allocation, garbage collection...
- scheduling: thread scheduling, blocking queues...
- IOs: primitives to interact with the operating system
- dynamic code loading: loading and unloading code objects at runtime.

The RTS itself comes in different flavors: e.g. using multiple OS threads to execute
Haskell code or not.

All these compilation stages and the RTS provide knobs to tweak the generated
code and the behavior of the runtime system. In particular, some probes can be
optionally inserted in the generated code at various stages to produce different
profiling information at runtime.


Execution model overview
------------------------

The heap of a Haskell program contain objects that reflect its current execution
state: suspended computations (thunks), partially applied functions (PAP),
values (datacon and their payload), threads (TSO) and their stack, etc.
When the computer executes the compiled code for a top-level STG binding
(starting from ``main``), it asks for more objects to be allocated in the heap,
for some thunks to be reduced to another heap object, or for existing objects to
be used as arguments for function applications.

The garbage collector is responsible for freeing space in the heap. It runs when
there is not enough space left or depending on other heuristics. GHC provides
several knobs to configure the garbage collector strategies to use and to tweak
their properties.

The garbage collector used has an impact on profiling. For an extreme example,
if the heap size is configured to be large enough than your program never has to
perfom garbage collection, you'll find that you spend 0% time doing garbage
collection and 100% executing your program ("mutator" time): all the garbage
collection occurs at once implicitly when your program exits. On the other hand,
if you configure the heap to be very small, most of the time can be spent doing
garbage collection even if you haven't changed anything in your program.
It means that **you must be very aware of the RTS options you use when profiling**.

Similarly, some profiling options have an impact on the size of the heap
objects: e.g. the heap object that represents `10 :: Int64` uses 24 bytes
instead of 16 bytes without profiling. Because of this, you may find that your
code with this profiling enabled triggers more garbage collections than your
code without this profiling enabled. **You have to be aware of the compilation
options you use when you make some runtime measurements.**

Consequences on Profiling
-------------------------

As a consequence of the Haskell compilation pipeline and of the Haskell execution model
we can measure many different things at different levels.
