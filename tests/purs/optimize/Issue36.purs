module Issue36 where

import Prelude

import Data.Eq ((==))

arrayPatternNoGuard :: Array Int -> Boolean
arrayPatternNoGuard = case _ of
    [ b, _ ] -> b == 3
    _ -> false

arrayMatch :: Array Int -> Boolean
arrayMatch = case _ of
    [ b, _ ] | b == 3 -> true
    _ -> false

doubleArrayMatchGuarded :: Array Int ->Array Int -> Boolean
doubleArrayMatchGuarded x y = case x, y of
    [ b, _ ], [ c ] | b > c -> true
    [ b, _ ], _ | b == 0 -> true
    _, _ -> false

data Tuple a b = Tuple a b

nonArrayMatch = case _ of
  Tuple b _ | b == 3 -> true
  _ -> false


doubleArrayPattern :: Tuple (Array Int) (Array Int) -> Boolean
doubleArrayPattern = case _ of
    Tuple [ b, _ ] _ -> b == 1
    Tuple _ [ _, c ] -> c == 32
    _ -> false

patternGuardArray = case _ of
    [ x ] |
      [ y ] <- x,
      [ z ] <- y,
      z > 0 ->
        true
    _ -> false