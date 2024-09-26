{-# OPTIONS_GHC -O2 -ddump-asm -ddump-cmm -ddump-to-file #-}

module Main where

import Data.Int

data Foo = Foo !Int16 !Int16

{-# NOINLINE my_foo #-}
my_foo :: Foo
my_foo = Foo 123 321

main :: IO ()
main = return ()
