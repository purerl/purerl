-- |
-- This module implements the "Magic Do" optimization, which inlines calls to return
-- and bind for the Eff monad, as well as some of its actions.
--
module Language.PureScript.Erl.CodeGen.Optimizer.MagicDo (magicDo) where

import Prelude.Compat

import Data.Text (Text)

import Language.PureScript.Erl.CodeGen.AST
import Language.PureScript.Erl.CodeGen.Optimizer.Common
import qualified Language.PureScript.Constants.Prelude as C
import qualified Language.PureScript.Erl.CodeGen.Constants as EC

magicDo :: (Erl -> Erl) -> Erl -> Erl
magicDo = magicDo'' EC.effect EC.effectDictionaries

magicDo'' :: Text -> EC.EffectDictionaries -> (Erl -> Erl) -> Erl -> Erl
magicDo'' effectModule EC.EffectDictionaries{..} expander =
  everywhereOnErlTopDown convert
  where
  -- The name of the function block which is added to denote a do block
  fnName = "__do"

  convert :: Erl -> Erl
  -- Desugar pure
  convert (EApp pure'@(EApp _ [_, val]) []) | isPure pure' = val
  -- same but curried
  convert (EApp pure'@((EApp _dictApp [val])) []) | isPure pure' = val

  -- Desugar discard
  convert discard@(EApp _ [_, _, m, EFun1 Nothing _ e]) | isDiscard discard =
    EFun0 (Just fnName) (EBlock (EApp m [] : [ EApp e [] ]))
  -- same but curried
  convert discard@(EApp( EApp _dictApp [m]) [EFun1 Nothing _ e]) | isDiscard discard =
    EFun0 (Just fnName) (EBlock (EApp m [] : [ EApp e [] ]))
  
  -- Desugar bind to wildcard
  convert bind'@(EApp _ [_, m, EFun1 Nothing "_" e]) | isBind bind' =
    EFun0 (Just fnName) (EBlock (EApp m [] : [ EApp e [] ]))
  -- same but curried
  convert bind'@(EApp (EApp _dictApp [m]) [EFun1 Nothing "_" e]) | isBind bind' =
    EFun0 (Just fnName) (EBlock (EApp m [] : [ EApp e [] ]))

  -- Desugar bind
  convert bind'@(EApp _ [_, m, EFun1 Nothing var e]) | isBind bind' =
    EFun0 (Just fnName) (EBlock (EVarBind var (EApp m []) : [ EApp e [] ]))
  -- same but curried
  convert bind'@(EApp (EApp _dictApp [m]) [EFun1 Nothing var e]) | isBind bind' =
    EFun0 (Just fnName) (EBlock (EVarBind var (EApp m []) : [ EApp e [] ]))

  -- TODO Inline double applications?
  convert other = other

  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (EApp fn [dict, _, _]) | isDict (effectModule, edBindDict) dict && isBindPoly fn = True
  isBind (EApp (EApp (expander -> EApp fn [dict]) [_]) [_]) | isDict (effectModule, edBindDict) dict && isBindPoly fn = True
  isBind _ = False

  -- Check if an expression represents a call to @discard@
  isDiscard (EApp fn [dict1, dict2, _, _])
    | isDict (EC.controlBind, C.discardUnitDictionary) dict1 && 
      isDict (effectModule, edBindDict) dict2 &&
      isDiscardPoly fn = True
  isDiscard (EApp (EApp (expander -> EApp fn [dict1, dict2]) [_]) [_])
    | isDict (EC.controlBind, C.discardUnitDictionary) dict1 && 
      isDict (effectModule, edBindDict) dict2 &&
      isDiscardPoly fn = True
  isDiscard _ = False

  -- Check if an expression represents a monomorphic call to pure or return for the Eff applicative
  isPure (EApp fn [dict, _]) | isDict (effectModule, edApplicativeDict) dict && isPurePoly fn = True
  isPure (EApp (expander -> EApp fn [dict]) [_]) | isDict (effectModule, edApplicativeDict) dict && isPurePoly fn = True
  isPure _ = False
  -- Check if an expression represents the polymorphic >>= function
  isBindPoly = isUncurriedFn (EC.controlBind, C.bind)
  -- Check if an expression represents the polymorphic pure or return function
  isPurePoly = isUncurriedFn (EC.controlApplicative, C.pure')
  -- Check if an expression represents the polymorphic discard function
  isDiscardPoly = isUncurriedFn (EC.controlBind, C.discard)
