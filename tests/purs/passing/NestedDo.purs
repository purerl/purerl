module NestedDo where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

bug26 :: Effect Unit
bug26 = do
  a <- do
    a <- do
      a <- pure unit
      pure a
    pure a
  log "Oh no"

renamed :: Effect Unit
renamed = do
  a2 <- do
    a1 <- do
      a0 <- pure unit
      pure a0
    pure a1
  log "Oh no"

renamed0 :: Effect Unit
renamed0 = do
  a <- do
    a <- do
      a0 <- pure unit
      pure a0
    pure a
  log "Oh no"

renamed1 :: Effect Unit
renamed1 = do
  do
    a <- do
      a <- pure unit
      pure a
    pure a
  log "Oh no"

renamed2 :: Effect Unit
renamed2 = do
  a <- do
    a1 <- do
      a <- pure unit
      pure a
    pure a1
  log "Oh no"


do0 :: Effect Unit
do0 = do
  a <- pure unit
  log "Oh no"

do1 :: Effect Unit
do1 = do
  a <- do
    a <- pure unit
    pure a
  log "Oh no" 
  
 
main :: Effect Unit
main = do
  bug26
  renamed
  renamed0
  renamed1
  renamed2
  do0
  do1
  log "Done"
