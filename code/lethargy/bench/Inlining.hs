-- |

module Main where

{-# OPTIONS_GHC -O2 -ddump-simpl -ddump-to-file -ddump-stg-final #-}

import Gauge



main :: IO ()
main = do
  defaultMain [ bgroup "SAT" [ bench "10"    $ nf (filter even) [1..10000 :: Int]
                             , bench "100"   $ nf (filter even) [1..100000 :: Int]
                             , bench "1000"  $ nf (filter even) [1..1000000 :: Int]
                             , bench "10000" $ nf (filter even) [1..10000000 :: Int]
                             ]
              ]
