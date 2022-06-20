module ForeignTypes (
  thing
  , test
) where

import Prelude

import Data.Maybe (Maybe)

foreign import test :: forall msg. Maybe msg  -> (Unit -> msg) -> Maybe Int

thing = 42