-- |
-- This module implements the "Magic Do" optimization, which inlines calls to pure
-- and bind for the Effect monad, as well as some of its actions.
--
-- General magic-do transformation & some of the cases taken from the JS backend 
--
module Language.PureScript.Erl.CodeGen.Optimizer.MagicDo (magicDo) where

import Prelude.Compat

import Data.Text (Text)

import Language.PureScript.Erl.CodeGen.AST
import Language.PureScript.Erl.CodeGen.Optimizer.Common
    ( isDict, isUncurriedFn, collect )
import qualified Language.PureScript.Constants.Prelude as C
import qualified Language.PureScript.Erl.CodeGen.Constants as EC
import Data.Bifunctor (Bifunctor(second))
import Protolude (unsnoc)

magicDo :: (Erl -> Erl) -> Erl -> Erl
magicDo = magicDo'' EC.effect EC.effectDictionaries

magicDo'' :: Text -> EC.EffectDictionaries -> (Erl -> Erl) -> Erl -> Erl
magicDo'' effectModule EC.EffectDictionaries{..} expander =
  everywhereOnErlTopDown convert
  where
  -- The name of the function block which is added to denote a do block
  fnName = "__do"

  convert :: Erl -> Erl
  convert appExp@EApp{} = case expander appExp of
    -- Desugar pure
    EApp (collect 2 -> EApp fn [dict, val]) [] | isPure fn dict -> val

    -- Desugar discard
    (collect 4 -> EApp fn [dict1, dict2, m, EFun1 Nothing _ e]) | isDiscard fn dict1 dict2 ->
      EFun0 (Just fnName) (EBlock (EApp m [] : [ EApp e [] ]))

    -- Desugar bind to wildcard
    (collect 3 -> EApp fn [dict, m, EFun1 Nothing "_" e]) | isBind fn dict ->
      EFun0 (Just fnName) (EBlock (EApp m [] : [ EApp e [] ]))

    -- Desugar bind
    (collect 3 -> EApp fn [dict, m, EFun1 Nothing var e]) | isBind fn dict ->
      EFun0 (Just fnName) (EBlock (EVarBind var (EApp m []) : [ EApp e [] ]))

    -- Desugar map
    EApp (collect 3 -> EApp fn [dict, f, m]) [] | isMap fn dict ->
      EApp f [ EApp m [] ]

    -- Push application under case
    EApp (ECaseOf ec binds) [] ->
      ECaseOf ec $ map (second (`EApp` [])) binds

    -- Push application under block
    EApp (EBlock es) [] 
      | Just (es', e) <- unsnoc es
      , all isVarBind es' ->
      EBlock (es' <> [ EApp e [] ])

    _other -> appExp


  -- TODO Inline double applications?

  -- ((fun () -> e)())()
  -- -->
  -- (fun () -> e())()

  -- what does that mean
  convert other = other

  isDiscard fn dict1 dict2 =
    isDict (EC.controlBind, C.discardUnitDictionary) dict1 &&
    isDict (effectModule, edBindDict) dict2 &&
    isDiscardPoly fn

-- Check if an expression represents a monomorphic call to >>= for the Effect monad
  isBind fn dict = isDict (effectModule, edBindDict) dict && isBindPoly fn

  -- Check if an expression represents a monomorphic call to pure or return for the Effect applicative
  isPure fn dict = isDict (effectModule, edApplicativeDict) dict && isPurePoly fn

  -- Check if an expression represents a monomorphic call to map for the Effect functor
  isMap fn dict = isDict (effectModule, edFunctor) dict && isMapPoly fn


  -- Check if an expression represents the polymorphic >>= function
  isBindPoly = isUncurriedFn (EC.controlBind, C.bind) . unthunk
  -- Check if an expression represents the polymorphic pure or return function
  isPurePoly = isUncurriedFn (EC.controlApplicative, C.pure') . unthunk
  -- Check if an expression represents the polymorphic discard function
  isDiscardPoly = isUncurriedFn (EC.controlBind, C.discard) . unthunk
  -- Check if an expression represents the polymorphic map function
  isMapPoly = isUncurriedFn (EC.dataFunctor, C.map) . unthunk

  unthunk (EApp fn []) = fn
  unthunk fn = fn

  isVarBind (EVarBind _ _) = True
  isVarBind _ = False