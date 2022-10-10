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

{-# LANGUAGE BangPatterns #-}
-- Need to disable optimizations because GHC will recognize and perform
-- let-floating for us!
{-# OPTIONS_GHC -O0 -ddump-simpl -ddump-to-file -ddump-stg-final #-}

module Main where

import Data.List              (foldl')
import System.Random          (mkStdGen)
import System.Random.Stateful (newIOGenM, uniformRM)
import Control.Concurrent     (threadDelay)
import Control.Monad          (replicateM)

-- ys :: [Int]
-- ys = [1..10000]

lazy_mean :: [Double] -> Double
lazy_mean xs = s / fromIntegral ln
  where (s, ln)        = foldl step (0,0) xs
        step (s, ln) a = (s + a, ln + 1)

stricter_mean :: [Double] -> Double
stricter_mean xs = s / fromIntegral ln
  where (s, ln)        = foldl' step (0,0) xs
        step (s, ln) a = (s + a, ln + 1)

strict_mean :: [Double] -> Double
strict_mean xs = s / fromIntegral ln
  where (s, ln)        = foldl' step (0,0) xs
        step (!s, !ln) a = (s + a, ln + 1)

main :: IO ()
main = do
  -- generate random test data
  seed <- newIOGenM (mkStdGen 1729)
  test_values <- replicateM 500000 $ uniformRM (0,500000) seed
  -- sleep for a second
  let wait = threadDelay 1000000
  -- now run
  print $! lazy_mean test_values
  wait
  print $! stricter_mean test_values
  wait
  print $! strict_mean test_values
