-----------------------------------------------------------------------------
-- |
-- Module      :  TooManyClosures/Main
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
-- Tested with GHC 8.10.7
-----------------------------------------------------------------------------


module Main where

import Gauge

-- | This function creates excessive closures. Once we move the infinite list
-- [0..] behind the lambda the list /will be created anew/ for each call of the
-- lambda.
bad :: (Int -> a) -> (Int -> a)
bad f = (\x -> (map f [0..] !!) x)

-- | This function /does not/ create excessive closures. It returns a function
-- that has allocated the infinite list '[0..]' only once, then when the
-- function is called this list is simply referenced.
good :: (Int -> a) -> (Int -> a)
good f = (map f [0..] !!)


main :: IO ()
main = let b = bad (+42)
           g = good (+42)
       in defaultMain [ bgroup "Too Many Closures" [ bench "bad"  $ whnf b 10
                                                   , bench "good" $ whnf g 10
                                                   ]
                   ]
