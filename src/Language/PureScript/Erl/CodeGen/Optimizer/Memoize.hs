module Language.PureScript.Erl.CodeGen.Optimizer.Memoize
  ( addMemoizeAnnotations,
  )
where

import Prelude

import Language.PureScript.Erl.CodeGen.AST
    ( Erl(..), Atom, everywhereOnErl )
import Data.Map (Map)
import Data.Map as Map ( lookup )

addMemoizeAnnotations :: Map Atom Int -> Erl -> Erl
addMemoizeAnnotations memoizable = everywhereOnErl go
  where
  go e = case e of
    EApp (EAtomLiteral f) args
      | Just n <- Map.lookup f memoizable
      , length args == n
      -> memoizeAnnotation e
    other -> other

memoizeAnnotation :: Erl -> Erl
memoizeAnnotation emem = EApp (EVar "?MEMOIZE") [emem]
