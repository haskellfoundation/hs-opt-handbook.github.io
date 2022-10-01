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

{-# OPTIONS_GHC -O0 -ddump-simpl -ddump-to-file -ddump-stg-final #-}

module Main where

import Gauge

-- ys :: [Int]
-- ys = [1..10000]

-- | This function excessively allocates closures every time 'f' is called. The
-- closure in question
bad :: [Int] -> Int
bad xs = sum $ fmap f xs
  where f x = x + length [1..10000]

good :: [Int] -> Int
good xs = sum $ fmap f xs
  where
    n = length [1..10000]
    f x = x + n



main :: IO ()
main = do
  let test_values = replicate 5000 1
  defaultMain [ bgroup "Too Many Closures" [ bench "bad"  $ whnf bad test_values
                                           , bench "good" $ whnf good test_values
                                           ]
                   ]
