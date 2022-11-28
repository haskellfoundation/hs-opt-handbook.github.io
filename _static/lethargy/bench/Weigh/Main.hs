{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Weigh
-- Copyright   :  (c) IOG 2022
-- License     :  CCC (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
-- Stability   :  stable
--
--
-- Module that demonstrates use of the weigh library
--
-- Tested with GHC 9.2.4
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -O2 -ddump-simpl -ddump-to-file -ddump-stg-final #-}

module Main where

import Data.List              (foldl')
import System.Random          (mkStdGen)
import System.Random.Stateful (newIOGenM, uniformRM)
import Control.Concurrent     (threadDelay)
import Control.Monad          (replicateM)
import Control.Exception      (evaluate)
import Control.DeepSeq
import GHC.Generics

import qualified Data.Map    as M
import qualified Data.IntMap as IM
import qualified Data.Set    as Set

import Weigh

i :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
i a b c d e f g h i j k = a + b + c + d + e + f + h + i + j + k

f :: Int -> Int -> Int -> Int -> Int -> Int -> Int
f a b c d e f = a + b + c + d + e + f

g :: Int -> Int -> Int -> Int -> Int -> Int
g a b c d e = a + b + c + d + e

count2 :: Int -> Int -> (Int,Int)
count2 a b = (a,b)

count3 :: Int -> Int -> (Int,Int,Int)
count3 a b = (a,b,a)

count4 :: Int -> Int -> (Int,Int,Int,Int)
count4 a b = (a,b,a,b)

count5 :: Int -> Int -> (Int,Int,Int,Int,Int)
count5 a b = (a,b,a,b,a)

count6 :: Int -> Int -> (Int,Int,Int,Int,Int,Int)
count6 a b = (a,b,a,b,a,b)

count9 :: Int -> Int -> (Int,Int,Int,Int,Int,Int,Int,Int,Int)
count9 a b = (a,b,a,b,a,a,b,a,b)

list_o_tuples :: [Int] -> [Int] -> [(Int,Int)]
list_o_tuples xs ys = [(x,y) | x <- xs, y <- ys]

data Foo0 = Foo0
  deriving (Generic,NFData)

data Foo1 = Foo1 Int
  deriving (Generic,NFData)

data Foo2 = Foo2 String String
  deriving (Generic,NFData)

one,two :: String
one = "one"
two = "two"

main :: IO ()
main = mainWith $ do
  value "()" ()
  value "1"  (1 :: Int)
  value "True"  True
  value "[0..3]"  ([0..3] :: [Int])
  value "[0,1,2,3]"  ([0,1,2,3] :: [Int])
  value "Foo0"  Foo0
  func  "Foo1-func"  Foo1 1
  value "Foo1-value"  (Foo1 1)
  value "one" one
  value "Foo2"  (Foo2 one two)
  -- value "([0,1,2],[3,4,5])"  ([[0..2], [3..5]] :: [[Int]])
  -- value "IntMap.Empty" (IM.empty :: IM.IntMap Int)
  -- value "Map.Empty"    (M.empty  :: M.Map Int Int)
  -- value "Set.Empty"    (Set.empty :: Set.Set Int)
  -- value "IntMap singleton" (IM.insert 2 IM.empty)
  -- value "Map.Empty"    (M.insert 1 2 mempty  :: M.Map Int Int)
  -- value "Set.Empty"    (Set.insert 2 mempty:: Set.Set Int)
  -- func "count2"  (count2 1) 2
  -- func "count3" (count3 1) 2
  -- func "count4" (count4 1) 2
  -- func "count5" (count5 1) 2
  -- func "count6" (count6 1) 2
  -- func "count9" (count9 1) 2
  -- func "ListOTups" (list_o_tuples [0..1]) [2..3]
