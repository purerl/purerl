module MagicDo where

import Prelude

import Effect
import Effect.Class
import Effect.Console
import MagicDo.EE

import Control.Monad.Reader.Trans

action :: Effect Unit
action = log "blah"

foo :: Effect Int
foo = do
  action
  foo <- pure 42
  action
  let bar = foo + 1
  pure 13

newtype Efect a = Efect (Effect a)

derive newtype instance Bind Efect
derive newtype instance Applicative Efect
derive newtype instance Monad Efect

action' :: Efect Unit
action' = Efect action

instance MonadEffect Efect where
  liftEffect = Efect

newt :: Efect Int
newt = do
  action'
  foo <- pure 42
  action'
  liftEffect action
  let bar = foo + 1
  pure 13

newtype E i = E (Effect i)
derive newtype instance functorE :: Functor E
derive newtype instance applyE :: Apply E
derive newtype instance applicativeE :: Applicative E
derive newtype instance bindE :: Bind E
derive newtype instance monadE :: Monad E
egE :: E Unit
egE = do
  E (log "abc")
  E (log "abc")
  E (log "abc")

ee :: EE Int
ee = do
  foo <- pure 42
  liftEffect action
  pure $ foo + 32

data MyEnv = MyEnv
type ER a = ReaderT MyEnv Effect a

er :: ER Int
er = do
  foo <- pure 42
  liftEffect action
  pure $ foo + 32
