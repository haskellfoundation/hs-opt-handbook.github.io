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
import Control.Exception      (evaluate)
import Control.DeepSeq        (force)

import Debug.Trace            (traceMarker, traceMarkerIO)

-- lazy_mean :: [Double] -> Double
-- lazy_mean xs = traceMarker "Begin: lazy_mean" $ s / fromIntegral ln
--   where (s, ln)        = foldl step (0,0) xs
--         step (s, ln) a = (s + a, ln + 1)

-- stricter_mean :: [Double] -> Double
-- stricter_mean xs = (traceMarker "s" s) / fromIntegral (traceMarker "ln" ln)
--   where (s, ln)        = foldl' step (0,0) xs
--         step (!s, !ln) a = (s + a, ln + 1)

-- strict_mean :: [Double] -> Double
-- strict_mean xs = traceMarker "Begin: strict_mean" $ s / fromIntegral ln
--   where (s, ln)        = foldl' step (0,0) xs
--         step (!s, !ln) a = (s + a, ln + 1)

-- main :: IO ()
-- main = do
--   let wait = threadDelay 100000
--   -- create a delay at the beginning of the program, if we don't do this then
--   -- our marker will be merged with the y-axis of the heap profile
--   wait
--   traceMarkerIO "Bench Initialization"
--   -- generate random test data
--   seed <- newIOGenM (mkStdGen 1729)
--   let genValue = fmap force uniformRM (0,500000) seed >>= evaluate           -- <--- new
--   test_values <- fmap force (replicateM 50000 genValue) >>= evaluate                        -- <--- new
--   -- let genValues = replicateM 500000 $ uniformRM (0,500000) seed          -- <--- new
--   -- test_values <- fmap force (replicateM 500000 $ uniformRM (0,500000) seed)
--   --                >>= evaluate
--   traceMarkerIO "End Bench Initialization"
--   wait
--   -- now run
--   -- print $! lazy_mean test_values
--   -- traceMarkerIO "End lazy_mean"
--   -- wait
--   traceMarkerIO "Begin stricter_mean"
--   print $! stricter_mean test_values
--   traceMarkerIO "End stricter_mean"
--   wait
--   print $! strict_mean test_values
--   traceMarkerIO "End strict_mean"

lazy_mean :: [Double] -> Double
lazy_mean xs = traceMarker "Begin: lazy_mean" $ s / fromIntegral ln
  where (s, ln)        = foldl step (0,0) xs
        step (s, ln) a = (s + a, ln + 1)

-- stricter_mean :: [Double] -> Double
-- stricter_mean xs = result
--   -- (traceMarker "s" s) / fromIntegral (traceMarker "ln" ln)
--   where (s, ln)        = foldl' step (0,0) xs
--         step (!s, !ln) a = (s + a, ln + 1)
--         !result = s / ln
stricter_mean :: [Double] -> Double
stricter_mean xs = (traceMarker "s" s) / fromIntegral (traceMarker "ln" ln)
  where (s, ln)        = foldl' step (0,0) xs
        step (!s, !ln) a = (s + a, ln + 1)

strict_mean :: [Double] -> Double
strict_mean xs = traceMarker "Begin: strict_mean" $ s / fromIntegral ln
  where (s, ln)        = foldl' step (0,0) xs
        step (!s, !ln) a = (s + a, ln + 1)

-- main :: IO ()
-- main = do
--   let wait = threadDelay 100000
--   -- create a delay at the beginning of the program, if we don't do this then
--   -- our marker will be merged with the y-axis of the heap profile
--   wait
--   traceMarkerIO "Bench Initialization"
--   -- generate random test data
--   seed <- newIOGenM (mkStdGen 1729)
--   test_values <- replicateM 500000 $ uniformRM (0,500000) seed
--   traceMarkerIO "End Bench Initialization"
--   wait
--   -- now run
--   print $! lazy_mean test_values
--   traceMarkerIO "End lazy_mean"
--   wait
--   print $! stricter_mean test_values
--   traceMarkerIO "End stricter_mean"
--   wait
--   print $! strict_mean test_values
--   traceMarkerIO "End strict_mean"

main :: IO ()
main = do
  let wait = threadDelay 100000
  -- create a delay at the beginning of the program, if we don't do this then
  -- our marker will be merged with the y-axis of the heap profile
  wait
  traceMarkerIO "Bench Initialization"
  -- generate random test data
  !seed <- newIOGenM (mkStdGen 1729)
  let genValue = fmap force uniformRM (0,500000) seed >>= evaluate -- new
  test_values <- replicateM 50000 genValue >>= evaluate . force -- new
  traceMarkerIO "End Bench Initialization"
  wait
  -- now run
  -- print $! lazy_mean test_values
  -- traceMarkerIO "End lazy_mean"
  -- wait
  traceMarkerIO "Begin stricter_mean"
  print $! stricter_mean test_values
  traceMarkerIO "End stricter_mean"
  wait
  print $! strict_mean test_values
  traceMarkerIO "End strict_mean"
