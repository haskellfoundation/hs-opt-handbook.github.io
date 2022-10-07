-----------------------------------------------------------------------------
-- |
-- Module      :  TooManyClosures
-- Copyright   :  (c) IOG 2022
-- License     :  CCC (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
-- Stability   :  stable
--
--
-- Module that shows the performance costs of excessive closure allocation See
-- 'The Programs of Consistent Lethargy' for a discussion of this code and
-- behavior.
--
-- Tested with GHC 9.2.4
-----------------------------------------------------------------------------

-- Need to disable optimizations because GHC will recognize and perform
-- let-floating for us!
{-# OPTIONS_GHC -O0 -ddump-simpl -ddump-to-file -ddump-stg-final #-}

module Main where

import Gauge

-- ys :: [Int]
-- ys = [1..10000]

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



main :: IO ()
main = do
  let test_values = replicate 5000 1
  putStrLn . show $ bad test_values
  -- defaultMain [ bgroup "Too Many Closures" [ bench "bad"  $ whnf bad test_values
  --                                          , bench "good" $ whnf good test_values
  --                                          ]
  --                  ]
