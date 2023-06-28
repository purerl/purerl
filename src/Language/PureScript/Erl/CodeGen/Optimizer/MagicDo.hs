-- |
-- This module implements the "Magic Do" optimization, which inlines calls to pure
-- and bind for the Effect monad, as well as some of its actions.
--
-- General magic-do transformation & some of the cases taken from the JS backend
module Language.PureScript.Erl.CodeGen.Optimizer.MagicDo (magicDo) where

import Data.Bifunctor (Bifunctor (second))
import Data.Text (Text)
import qualified Language.PureScript.Constants.Libs as C
import Language.PureScript.Erl.CodeGen.AST
import qualified Language.PureScript.Erl.CodeGen.Constants as EC
import Language.PureScript.Erl.CodeGen.Optimizer.Common
  ( collect,
    isDict,
    isUncurriedFn,
  )
import Prelude.Compat
import Protolude (unsnoc)

magicDo :: (Erl -> Erl) -> Erl -> Erl
magicDo = magicDo'' EC.effect EC.effectDictionaries

magicDo'' :: Text -> EC.EffectDictionaries -> (Erl -> Erl) -> Erl -> Erl
magicDo'' effectModule EC.EffectDictionaries {..} expander =
  everywhereOnErlTopDown convert
  where
    -- The name of the function block which is added to denote a do block
    fnName = "__do"

    convert :: Erl -> Erl
    convert appExp@EApp {} = case expander appExp of
      -- Desugar pure
      EApp _ (collect 2 -> EApp _ fn [dict, val]) [] | isPure fn dict -> val
      -- Desugar discard
      (collect 4 -> EApp _ fn [dict1, dict2, m, EFun1 Nothing _ e])
        | isDiscard fn dict1 dict2 ->
          EFun0 (Just fnName) (EBlock (EApp RegularApp m [] : [EApp RegularApp e []]))
      -- Desugar bind to wildcard
      (collect 3 -> EApp _ fn [dict, m, EFun1 Nothing "_" e])
        | isBind fn dict ->
          EFun0 (Just fnName) (EBlock (EApp RegularApp m [] : [EApp RegularApp e []]))
      -- Desugar bind
      (collect 3 -> EApp _ fn [dict, m, EFun1 Nothing var e])
        | isBind fn dict ->
          EFun0 (Just fnName) (EBlock (EVarBind var (EApp RegularApp m []) : [EApp RegularApp e []]))
      -- Desugar map
      EApp _ (collect 3 -> EApp _ fn [dict, f, m]) []
        | isMap fn dict ->
          EApp RegularApp f [EApp RegularApp m []]
      -- Push application under case
      EApp _ (ECaseOf ec binds) [] ->
        ECaseOf ec $ map (second (flip (EApp RegularApp) [])) binds
      -- Push application under block
      EApp _ (EBlock es) []
        | Just (es', e) <- unsnoc es,
          all isVarBind es' ->
          EBlock (es' <> [EApp RegularApp e []])
      _other -> appExp

    -- TODO Inline double applications?

    -- ((fun () -> e)())()
    -- -->
    -- (fun () -> e())()

    convert other = other

    isDiscard fn dict1 dict2 =
      isDict (EC.controlBind, snd C.P_discardUnit) dict1
        && isDict (effectModule, edBindDict) dict2
        && isDiscardPoly fn

    -- Check if an expression represents a monomorphic call to >>= for the Effect monad
    isBind fn dict = isDict (effectModule, edBindDict) dict && isBindPoly fn

    -- Check if an expression represents a monomorphic call to pure or return for the Effect applicative
    isPure fn dict = isDict (effectModule, edApplicativeDict) dict && isPurePoly fn

    -- Check if an expression represents a monomorphic call to map for the Effect functor
    isMap fn dict = isDict (effectModule, edFunctor) dict && isMapPoly fn

    -- Check if an expression represents the polymorphic >>= function
    isBindPoly = isUncurriedFn (EC.controlBind, C.S_bind) . unthunk
    -- Check if an expression represents the polymorphic pure or return function
    isPurePoly = isUncurriedFn (EC.controlApplicative, C.S_pure) . unthunk
    -- Check if an expression represents the polymorphic discard function
    isDiscardPoly = isUncurriedFn (EC.controlBind, C.S_discard) . unthunk
    -- Check if an expression represents the polymorphic map function
    isMapPoly = isUncurriedFn (EC.dataFunctor, C.S_map) . unthunk

    unthunk (EApp _ fn []) = fn
    unthunk fn = fn

    isVarBind (EVarBind _ _) = True
    isVarBind _ = False
