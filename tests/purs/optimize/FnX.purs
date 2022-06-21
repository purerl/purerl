module FnX where

import Prelude

import Data.Function.Uncurried (Fn3, mkFn3, runFn3)

uncurried :: Fn3 Int Int Int Int
uncurried = mkFn3 \a b c -> a+b+c

uncurriedFn :: Fn3 Int Int Int (Int -> Int)
uncurriedFn = mkFn3 \a b c -> \d -> a+b+c+d


curried :: Int -> Int -> Int -> Int
curried a b c = a+b+c

main = runFn3 uncurriedFn 1 2 3 4