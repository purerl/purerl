module MagicDo.EE where

import Prelude

import Effect
import Effect.Class

newtype EE i = EE (Effect i)
derive newtype instance Functor EE
derive newtype instance Apply EE
derive newtype instance Applicative EE
derive newtype instance Bind EE
derive newtype instance Monad EE
derive newtype instance MonadEffect EE