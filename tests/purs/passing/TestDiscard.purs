module TestDiscard where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class.Console (log, logShow)

f :: forall m. Applicative m => m Int
f = pure 42

test :: forall m. Monad m => m Int
test = do
  pure unit
  x <- f
  v7 <- f
  pure unit
  pure x

testMaybe :: Maybe Int
testMaybe = do
  pure unit
  x <- f
  y <- f
  pure unit *> pure x


main :: Effect Unit
main = do
  logShow (test :: Maybe Int)
  logShow testMaybe
  log "Done"
