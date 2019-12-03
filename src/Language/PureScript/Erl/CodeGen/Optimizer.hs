-- |
-- This module optimizes code in the simplified-Erlang intermediate representation.
--
-- The following optimizations are supported:
--
--  * Inlining of (>>=) and ret for the Eff monad
--
module Language.PureScript.Erl.CodeGen.Optimizer (optimize) where

import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply)

import Language.PureScript.Erl.CodeGen.AST
import Language.PureScript.Erl.CodeGen.Optimizer.MagicDo
import Language.PureScript.Erl.CodeGen.Optimizer.Blocks
import Language.PureScript.Erl.CodeGen.Optimizer.Common
import Language.PureScript.Erl.CodeGen.Optimizer.Inliner
import Language.PureScript.Erl.CodeGen.Optimizer.Guards

-- |
-- Apply a series of optimizer passes to simplified Javascript code
--

optimize :: MonadSupply m => Erl -> m Erl
optimize erl = do
    erl' <- untilFixedPoint (tidyUp . applyAll
      [ 
        inlineCommonValues
      , inlineCommonOperators
      ]
      ) erl
    untilFixedPoint tidyUp
      =<< untilFixedPoint (return . magicDo')
      =<< untilFixedPoint (return . magicDo) 
      erl'

  where
  tidyUp :: MonadSupply m => Erl -> m Erl
  tidyUp = applyAllM
    [ pure . collapseNestedBlocks
    , pure . inlineSimpleGuards
    , pure . evaluateIifes
    , etaConvert
    ]


untilFixedPoint :: (Monad m, Eq a) => (a -> m a) -> a -> m a
untilFixedPoint f = go
  where
  go a = do
   a' <- f a
   if a' == a then return a' else go a'
