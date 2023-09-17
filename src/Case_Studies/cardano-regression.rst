.. _cardano regression case study:

..
   Local Variables
.. |c-l| replace:: `cardano-ledger <https://github.com/input-output-hk/cardano-ledger/>`__
.. |new| replace:: GHC-9.2.8
.. |old| replace:: GHC-8.10.7
.. |inline|     replace:: ``INLINE``
.. |inlineable| replace:: ``INLINEABLE``
.. |spec|       replace:: ``SPECIALIZE``


`Cardano-Ledger: Performance Regression Updating from GHC-8.10.7 to GHC-9.2.8`
==============================================================================

This chapter is a case study on a performance regression in the |c-l| code base
that IOG observed when upgrading the code base from |old| to |new|. To root
cause the performance regression this case study directly inspects the
:ref:`Core <Reading Core>` and uses the GHC :ref:`Profiler <GHC Flags>`. After
reading this chapter, one should be able to spot inefficient Core, understand
the difference and use cases for |inline|, |inlineable| and |spec| pragmas.

The rest of the chapter is structured as follows. We introduce evidence of the
performance regression. From this information we choose candidates to inspect as
leads in our investigation. TODO :math:`\ldots{}`


Evidence of a Regression
------------------------

The regression was first observed in an integration test performed by the
Cardano Benchmark team which resulted in two GHC Profiles:

One for |old|:

.. image:: /_static/cardano-regression/8107_perf.png
   :width: 800

And one for |new|:

.. image:: /_static/cardano-regression/927_perf.png
   :width: 800

First, notice the difference in ``total alloc`` at the top of the report
summaries. |old| shows total allocations of ~157GB, while |new| shows total
allocations around ~220GB; a 40% increase.

Next, observe that two :term:`CAF`'s have changed position in the summary:
``size`` from ``Cardano.Ledger.UMap`` and ``updateStakeDistribution`` from
``Cardano.Ledger.Shelley.LedgerState.IncrementalStake``. These two functions
will be our guides to understanding the regression. In the spirit of :ref:`Don't
think, look <Don't think, look>`, we'll compare the Core output between |old|
and |new|.

Understanding the Cardano.Ledger.UMap.size regression
-----------------------------------------------------

Here is the Core output on |new|:

.. code-block:: haskell

   -- RHS size: {terms: 22, types: 63, coercions: 0, joins: 0/0}
   size :: forall c k v. UView c k v -> Int
   [GblId,
    Arity=1,
    Str=<1L>,
    Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
            WorkFree=True, Expandable=True,
            Guidance=ALWAYS_IF(arity=1,unsat_ok=True,boring_ok=False)
   ...
   size
     = \ (@c_aviN)
         (@k_aviO)
         (@v_aviP)
         (ds_dAfr :: UView c_aviN k_aviO v_aviP) ->
         case ds_dAfr of wild_Xe {
           __DEFAULT ->
             Cardano.Ledger.UMap.$fFoldableUView_$cfoldl'
               @c_aviN
               @k_aviO
               @Int
               @v_aviP
               (Cardano.Ledger.UMap.size2 @v_aviP)
               Cardano.Ledger.UMap.size1
               wild_Xe;
           PtrUView co_aviQ [Dmd=A] co1_aviR [Dmd=A] ds1_dAiu ->
             case ds1_dAiu of { UMap ds2_sJNa ds3_sJNb ->
             case ds3_sJNb of {
               Data.Map.Internal.Bin dt_iAio ds4_iAip ds5_iAiq ds6_iAir
                                     ds7_iAis ->
                 ghc-prim:GHC.Types.I# dt_iAio;
               Data.Map.Internal.Tip -> Cardano.Ledger.UMap.size1
             }
             }
         }

.. note::

   I've elided the :term:`Unfolding` for ``size`` and only present the
   ``IdInfo`` for the term. Unfoldings are important to inspect and understand,
   but for our purposes the unfoldings are simply copies of the function body.
   See :ref:`Unfoldings <Reading Core>` in the Reading Core chapter. For our
   purposes, unless stated otherwise all Core will be generated with
   ``-ddump-simpl`` and no suppression flags. This is purposefully done to show
   what Core in a real project can look like.


On |old| the Core is slightly different:


.. code-block:: haskell

    size :: forall c k v. UView c k v -> Int
    [GblId,
    Arity=1,
    Caf=NoCafRefs,
    Str=<S,1*U>,
    Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
            WorkFree=True, Expandable=True, Guidance=IF_ARGS [70] 100 20}]
    size
    = \ (@ c_a7SFB)
        (@ k_a7SFC)
        (@ v_a7SFD)
        (ds_d7UZd :: UView c_a7SFB k_a7SFC v_a7SFD) ->
        case ds_d7UZd of wild_Xfk {
            __DEFAULT ->
            Cardano.Ledger.UMap.size_$cfoldl'
                @ c_a7SFB
                @ k_a7SFC
                @ Int
                @ v_a7SFD
                (Cardano.Ledger.UMap.size2 @ v_a7SFD)
                Cardano.Ledger.UMap.size1
                wild_Xfk;
            PtrUView co_a7SFF [Dmd=<L,A>] co1_a7SFG [Dmd=<L,A>] ds1_d7Vel ->
            case ds1_d7Vel of { UMap ds2_s90fe ds3_s90ff ->
            case ds3_s90ff of {
                Data.Map.Internal.Bin dt_a7UZH ds4_a7UZI ds5_a7UZJ ds6_a7UZK
                                    ds7_a7UZL ->
                ghc-prim-0.6.1:GHC.Types.I# dt_a7UZH;
                Data.Map.Internal.Tip -> Cardano.Ledger.UMap.size1
            }
            }
        }

Notice that on |new| the ``DEFAULT`` case calls
``Cardano.Ledger.UMap.$fFoldableUView_$cfoldl'`` whereas on |old| this call is
``Cardano.Ledger.UMap.size_$cfoldl'``. Let's check these functions:

|new|:

.. code-block:: haskell

   -- RHS size: {terms: 215, types: 375, coercions: 57, joins: 0/4}
   Cardano.Ledger.UMap.$fFoldableUView_$cfoldl'
     :: forall c k b a. (b -> a -> b) -> b -> UView c k a -> b
   [GblId, Arity=3, Str=<LCL(C1(L))><1L><1L>, Unf=OtherCon []]
   Cardano.Ledger.UMap.$fFoldableUView_$cfoldl'
     = \ (@c_a2svV)
         (@k_a2svW)
         (@b_a2szt)
         (@a_a2szu)
         (accum_a2plt :: b_a2szt -> a_a2szu -> b_a2szt)
         (ans0_a2plu :: b_a2szt)
         (ds_d2xJs :: UView c_a2svV k_a2svW a_a2szu) ->
         case ds_d2xJs of {
           RewDepUView co_a2szv [Dmd=A] co1_a2szw ds1_d2xTB ->
             case ds1_d2xTB of { UMap ds2_s2BfK ds3_s2BfL ->
             letrec {
               go15_s2zs0 [Occ=LoopBreaker, Dmd=SCS(C1(L))]
                 :: b_a2szt
                    -> Map (Credential 'Staking c_a2svV) (UMElem c_a2svV) -> b_a2szt
               [LclId, Arity=2, Str=<1L><1L>, Unf=OtherCon []]
               go15_s2zs0
                 = \ (z'_i2wnP :: b_a2szt)
                     (ds4_i2wnQ
                        :: Map (Credential 'Staking c_a2svV) (UMElem c_a2svV)) ->
                     case ds4_i2wnQ of {
                       Data.Map.Internal.Bin ipv_i2wnT ipv1_i2wnU ipv2_i2wnV ipv3_i2wnW
                                             ipv4_i2wnX ->
                         case go15_s2zs0 z'_i2wnP ipv3_i2wnW of z''_i2wnZ { __DEFAULT ->
                         case (umElemRDPair @c_a2svV ipv2_i2wnV)
                         ...

|old|:

.. code-block:: haskell

   -- RHS size: {terms: 272, types: 431, coercions: 77, joins: 0/4}
   Cardano.Ledger.UMap.size_$cfoldl'
     :: forall c k b a. (b -> a -> b) -> b -> UView c k a -> b
   [GblId,
    Arity=3,
    Caf=NoCafRefs,
    Str=<L,C(C1(U))><S,1*U><S,1*U>,
    Unf=OtherCon []]
   Cardano.Ledger.UMap.size_$cfoldl'
     = \ (@ c_a7TVW)
         (@ k_a7TVX)
         (@ b_a7TZI)
         (@ a_a7TZJ)
         (accum_a7RPi :: b_a7TZI -> a_a7TZJ -> b_a7TZI)
         (ans0_a7RPj :: b_a7TZI)
         (ds_d8v9s :: UView c_a7TVW k_a7TVX a_a7TZJ) ->
         case ds_d8v9s of {
           RewDepUView co_a7TZL [Dmd=<L,A>] co1_a7TZM ds1_d8wpq ->
             case ds1_d8wpq of { UMap ds2_s90eY ds3_s90eZ ->
             letrec {
               go15_s8G6Q [Occ=LoopBreaker]
                 :: b_a7TZI
                    -> Map (Credential 'Staking c_a7TVW) (UMElem c_a7TVW) -> b_a7TZI
               [LclId, Arity=2, Str=<S,1*U><S,1*U>, Unf=OtherCon []]
               go15_s8G6Q
                 = \ (z'_a8iQB :: b_a7TZI)
                     (ds4_a8iQC
                        :: Map (Credential 'Staking c_a7TVW) (UMElem c_a7TVW)) ->
                     case ds4_a8iQC of {
                       Data.Map.Internal.Bin ipv_a8iQF ipv1_a8iQG ipv2_a8iQH ipv3_a8iQI
                                             ipv4_a8iQJ ->
                         case go15_s8G6Q z'_a8iQB ipv3_a8iQI of z''_a8iQL { __DEFAULT ->
                         case ipv2_a8iQH of {
                           __DEFAULT -> go15_s8G6Q z''_a8iQL ipv4_a8iQJ;
                           TFEEE dt_d8BOJ dt1_d8BOK ->


These functions are again nearly identical. Both define a function which inputs
four type variables , and three term variables, and then defines a local
function called with a recursive let. For example on |old| we have: ``c_a7TVW``,
``k_a7TVX``, ``b_a7TZI``, and ``a_a7TZJ`` for type variables, ``accum_a7RPi``,
``ans0_a7RPj``, and ``ds_d8v9s`` for term variables, and ``go15_s8G6Q`` for the
local recursive function.

From the summary comment above the function signature we can see that
``cfoldl'`` on |old| is larger (272 terms) compared to |new| (215 terms). Now
larger Core *is not always* worse than smaller Core; it depends on
specialization and inlining behavior. In this case, the larger Core is a better
performing program. On |old| we can see that the local function ``go15`` begins
pattern matching on an :term:`Algebraic Data Type`.
