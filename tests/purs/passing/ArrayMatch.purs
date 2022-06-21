module ArrayMatch where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  log $ show $ bug28 { q: [1, 2 ] }
  log $ show $ bug28_2 { q: [1, 2 ] }
  log $ show $ nestedArray [ [ 1, 2 ], [ 3 ]]
  log $ show $ nestedArrayViaRecord { q: [ { r: [ 1, 2 ] }, { r: [ 3 ] } ] }
  log $ show $ onlyArray [1]
  log $ show $ maybeArray $ Just [1 , 2 ]
  log $ show $ namedArray [1, 2]
  log "Done"

bug28 a = 
  case a of 
    { q: [a,b] } -> a + b
    _ -> 0

bug28_2 a = case { q: [ 1, 2 ] } of
  { q: s } -> case s of
    [ x, y ] -> x + y
    _ -> 0
  _ -> 0

nestedArray = 
  case _ of 
    [ [a,b], _ ] -> a + b + 1 
    -- [ [c], _ ] -> c
    _ -> 0

nestedArrayViaRecord = 
  case _ of 
    { q: [ { r: [ a, b ]}] } -> a + b
    { q: [ { r: [ a ]}, { r: [ b ] } ] } -> a + b
    _ -> 0

nestedArrayRefutable arg1 arg2 = 
  case arg1, arg2 of 
    [ [a,b,3], _ ], 2 -> a + b + 1 
    -- [ [c], _ ] -> c
    _, _ -> 0

nestedArrayRefutable2 = 
  case _, _ of 
    [ [a,b,3], _ ], 2 -> a + b + 1 
    -- [ [c], _ ] -> c
    _, _ -> 0


onlyArray a = case a of
  [ a, b ] -> a + b
  _ -> 0

maybeArray a = case a of
  Just [ a, b ] -> a + b
  _ -> 0


namedArray = case _ of
  foo@[a,b] -> a+b
  _ -> 0






-- -- 