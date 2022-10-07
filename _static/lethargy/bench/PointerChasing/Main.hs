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
import Data.List              (foldl')
import System.Random          (mkStdGen)
import System.Random.Stateful (newIOGenM, uniformRM)
import Control.Monad          (replicateM)

-- ys :: [Int]
-- ys = [1..10000]

lazy_mean :: [Double] -> Double
lazy_mean xs = s / fromIntegral ln
  where (s, ln)        = foldl step (0,0) xs
        step (s, ln) a = (s + a, ln + 1)

better_mean :: [Double] -> Double
better_mean xs = s / fromIntegral ln
  where (s, ln)        = foldl' step (0,0) xs
        step (s, ln) a = (s + a, ln + 1)


main :: IO ()
main = do
  seed <- newIOGenM (mkStdGen 1729)
  test_values <- replicateM 5000 $ uniformRM (0,5000) seed
  defaultMain [ bgroup "PointerChasing" [ bench "lazy"   $ whnf lazy_mean   test_values
                                        , bench "better" $ whnf better_mean test_values
                                        ]
                   ]
