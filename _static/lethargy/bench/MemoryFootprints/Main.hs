{-# OPTIONS_GHC -O2 -ddump-asm -ddump-to-file -ddump-cmm -ddump-stg-final #-}

module Main where

import Data.Int
import Data.Word

{-# NOINLINE a_unit #-}
a_unit :: ()
a_unit =  ()

{-# NOINLINE a_char #-}
a_char :: Char
a_char = 'a'

{-# NOINLINE a_pair#-}
a_pair :: (Int,Int)
a_pair =  (1,2)

{-# NOINLINE a_list#-}
a_list :: [Int16]
a_list =  [1,2,3,4]

{-# NOINLINE a_bool #-}
a_bool :: Bool
a_bool = True

{-# NOINLINE a_int8 #-}
a_int8 :: Int8
a_int8 = 8

{-# NOINLINE a_int16 #-}
a_int16 :: Int16
a_int16 = 16

{-# NOINLINE a_int #-}
a_int :: Int
a_int = 32

{-# NOINLINE a_int64 #-}
a_int64 :: Int64
a_int64 = 64

{-# NOINLINE a_word8 #-}
a_word8 :: Word8
a_word8 = 8

{-# NOINLINE a_word16 #-}
a_word16 :: Word16
a_word16 = 16

{-# NOINLINE a_word #-}
a_word :: Word
a_word = 64

{-# NOINLINE a_word64 #-}
a_word64 :: Word64
a_word64 = 64

{-# NOINLINE a_pair_result #-}
a_pair_result :: Int
a_pair_result = fst a_pair + snd a_pair

main :: IO ()
main = print a_pair_result

data Foo = Foo !Word16 Word16 Word16 Word16

a_foo :: Foo
a_foo = Foo 123 234 345 456
