.. _Heap Profiling Chapter:

:lightgrey:`GHC Flags`
======================

`TODO <https://github.com/input-output-hk/hs-opt-handbook.github.io/issues/22>`_

..
   The Running Example
   -------------------

   Our test program is an example of excessive closure allocation:

   .. code-block:: haskell

       -- Need to disable optimizations because GHC will recognize and perform
       -- let-floating for us!
       {-# OPTIONS_GHC -O0 -ddump-simpl -ddump-to-file -ddump-stg-final #-}

       module Main where

       import Gauge

       -- | This function excessively allocates closures every time 'f' is called. The
       -- closure in question being the allocation of the 10k element list.
       bad :: [Int] -> Int
       bad xs = sum $ fmap f xs
         where f x = x + length [1..10000]

       -- | This function avoids the excessive closure allocation. We still will
       -- allocate thunks for every element of 'xs' but we only calculate 'length
       -- [1..10000]' once because we floated it out of 'f'.
       good :: [Int] -> Int
       good xs = sum $ fmap f xs
         where
           n = length [1..10000]
           f x = x + n

       -- | use Gauge to run the benchmarks
       main :: IO ()
       main = do
         let test_values = replicate 5000 1
         defaultMain [ bgroup "Too Many Closures" [ bench "bad"  $ whnf bad test_values
                                                  , bench "good" $ whnf good test_values
                                                  ]
                     ]
