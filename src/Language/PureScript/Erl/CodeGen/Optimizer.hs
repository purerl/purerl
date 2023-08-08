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
    ( everywhereOnErl, Erl(..), pattern EApp, Atom, curriedApp )
import Language.PureScript.Erl.CodeGen.Optimizer.MagicDo
    ( magicDo )
import Language.PureScript.Erl.CodeGen.Optimizer.Blocks
    ( collapseNestedBlocks )
import Language.PureScript.Erl.CodeGen.Optimizer.Common
    ( applyAll, applyAllM )
import Language.PureScript.Erl.CodeGen.Optimizer.Inliner
    ( beginBinds,
      etaConvert,
      evaluateIifes,
      inlineCommonOperators,
      inlineCommonValues,
      singleBegin, collectLists, replaceAppliedFunRefs, inlineCommonFnsM )
import Language.PureScript.Erl.CodeGen.Optimizer.Guards
    ( inlineSimpleGuards )

import qualified Language.PureScript.Erl.CodeGen.Constants as EC
import Language.PureScript.Erl.CodeGen.Optimizer.Unused (removeUnusedFuns)
import Data.Map (Map)
import Language.PureScript.Erl.CodeGen.Optimizer.Memoize (addMemoizeAnnotations)
import Control.Monad ((<=<))
-- |
-- Apply a series of optimizer passes to simplified Javascript code
--
optimize :: MonadSupply m => [(Atom, Int)] -> Map Atom Int -> [Erl] -> m [Erl]
optimize exports memoizable es =
  removeUnusedFuns exports <$> traverse go es
  where
  go erl =
   do
    erl' <- untilFixedPoint (tidyUp <=< inlineCommonFnsM expander . applyAll
      [ 
        inlineCommonValues expander
      , inlineCommonOperators EC.effect EC.effectDictionaries expander
      ]
      ) erl
    erl'' <- untilFixedPoint tidyUp
      =<< untilFixedPoint (return . magicDo expander) 
      erl'

    pure $ addMemoizeAnnotations memoizable erl''

  expander = buildExpander es

  tidyUp :: MonadSupply m => Erl -> m Erl
  tidyUp = applyAllM
    [ pure . collapseNestedBlocks
    , pure . inlineSimpleGuards
    , pure . beginBinds
    , pure . evaluateIifes
    , pure . singleBegin
    , pure . replaceAppliedFunRefs
    , pure . collectLists
    , etaConvert
    ]


untilFixedPoint :: (Monad m, Eq a) => (a -> m a) -> a -> m a
untilFixedPoint f = go
  where
  go a = do
   a' <- f a
   if a' == a then return a' else go a'


-- |
-- Take all top-level ASTs and return a function for expanding top-level
-- variables during the various inlining steps in `optimize`.
--
-- Everything that gets inlined as an optimization is of a form that would
-- have been lifted to a top-level binding during CSE, so for purposes of
-- inlining we can save some time by only expanding variables bound at that
-- level and not worrying about any inner scopes.
--
buildExpander :: [Erl] -> Erl -> Erl
buildExpander = replaceUpdates . foldr go []
  where
  go = \case
    EFunctionDef _ _ name [] e | isSimpleApp e && size e <= 5 -> ((name, e) :)
    _ -> id
  
  replaceUpdates updates = everywhereOnErl (replaceUpdate updates)
  
  replaceUpdate updates = \case
    -- Regular form e.g. bind()
    EApp _ (EAtomLiteral a) [] | Just e <- lookup a updates
      -> replaceUpdates updates e
    -- Uncurried form, e.g. bind(e1, e2) where bind is the overload for bind/2, bind/0, such that bind(e1, e2) ~ ((bind())(e1))(e2)
    -- IF the original replacement was valid by way of being eligible to be inlined, this one should be too, doing both steps in 1
    
    EApp _ (EAtomLiteral a) args | Just e <- lookup a updates
      -> replaceUpdates updates $ curriedApp args e

    other -> other

  -- simple nested applications that look similar to floated synthetic apps
  isSimpleApp (EApp _ e1 es) = isSimpleApp e1 && all isSimpleApp es
  isSimpleApp (EAtomLiteral _) = True
  isSimpleApp _ = False

  -- Doesn't particularly matter what the size metric is, just that we have empirically determined
  -- a cutoff for inlining. This is per-definition, so technically we could come up with a case where
  -- we inline a huge expression because it is never large at any step and we recursively inline - but this
  -- doesn't seem a problem currently.
  size (EApp _ e1 es) = size e1 + sum (size <$> es)
  size _ = 1 :: Integer
