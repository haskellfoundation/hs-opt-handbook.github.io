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
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -ddump-stg-final
#-}

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

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Weigh

-- f,f',g_lifted :: Int -> Int -> Int
-- f a 0 = a
-- f a n = f (g (n `mod` 2)) (n - 1)
--   where g 0 = a
--         g n = 1 + g (n - 1)

-- f' a 0 = a
-- f' a n = f' (g_lifted a (n `mod` 2)) (n - 1)

-- g_lifted a 0 = a
-- g_lifted a n = 1 + g_lifted a (n - 1)

-- list_o_tuples :: [Int] -> [Int] -> [(Int,Int)]
-- list_o_tuples xs ys = [(x,y) | x <- xs, y <- ys]

-- data Foo0 = Foo0
--   deriving (Generic,NFData)

-- data Foo1 = Foo1 Int
--   deriving (Generic,NFData)

-- data Foo2 = Foo2 String String
--   deriving (Generic,NFData)

-- data Foo3 = One Int
--           | Two Int Int
--           | TTT Int
--           | TTTT Int
--           | TTTTT Int
--           | TTTTTT Int
--           | TTTTTTT Int
--           | TTTTTTTT Int
--           | TTTTTTTTT Int
--           | TTTTTTTTTT Int
--           | TTTTTTTTTTT Int
--           | TTTTTTTTTTTT Int
--           | TTTTTTTTTTTTT Int
--           deriving (Generic,NFData)

-- data SingleCons = SingleCons Int
--           deriving (Generic,NFData)

-- data TwoConsReg = FirstCon Int
--                 | SecondCon Int
--           deriving (Generic,NFData)

data LotsOfInts  = A Int Int
                 | B Int Int
                 | C Int Int
                 | D Int Int
                 | E Int Int
          deriving (Generic,NFData)

data LotsOfInts2 = A2 Int Int
                 | B2 Int Int
                 | C2 Int Int
                 | D2 Int Int
                 | E2 Int Int
                 | F2 Int Int
                 | G2 Int Int
                 | H2 Int Int
                 | I2 Int Int
          deriving (Generic,NFData)

-- one,two :: String
-- one = "one"
-- two = "two"

-- type Mytuple = (Int, Int)

-- myTuple :: Mytuple
-- myTuple = (3, 4)

-- text_one :: T.Text
-- text_one = T.pack one

-- x,y :: Int

-- x = f 100 10000
-- y = f' 100 10000

  -- value "()" ()
  -- value "1"  (1 :: Int)
  -- value "True"  True
  -- value "[0..3]"  ([0..3] :: [Int])
  -- value "[0,1,2,3]"  ([0,1,2,3] :: [Int])
  -- value "Foo0"  Foo0
  -- func  "Foo1-func"  Foo1 1
  -- value "Foo1-value"  (Foo1 1)
  -- value "Foo3-value"  (  TTTTTTTTTTTTT 1)
main :: IO ()
main = mainWith $ do
  setColumns [Case, Allocated, Max, Live, GCs, MaxOS]
  value "value: A Lot Of Ints"  (A 1000 1001)
  func  "func:  A Lot Of Ints"  (A 1000) 1001
  value "value: A Lot Of Ints2" (A2 1000 1001)
  func  "func:  A Lot Of Ints2" (A2 1000) 1001
  -- value "SingleCons" (SingleCons 1729)
  -- value "Irregular" (E 1)
  -- value "1" (1 :: Int)
  -- value "Foo2"  (Foo2 one two)
  -- value "myTuple" myTuple
  -- value "one" one
  -- func  "f"  (f 5) 10000
  -- func  "f'"  (f 5) 10000
  -- value  "fx"  x
  -- value  "fy"  y
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
  -- func "text strict" T.pack "very-long-text"
  -- value "text lazy" (TL.pack "a")
  -- value "text_string" "'"
  -- value "text_one" text_one
  -- value "string_one" one
  -- value "a char" '.'
