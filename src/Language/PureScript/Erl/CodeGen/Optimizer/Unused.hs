-- | Removes unused variables
module Language.PureScript.Erl.CodeGen.Optimizer.Unused
  ( removeUnusedFuns,
  )
where

import Control.Monad (filterM)
import Data.Monoid (Any (..))
import qualified Data.Set as S
import Data.Text (Text)
import Language.PureScript.Erl.CodeGen.AST
  ( Atom (..),
    Erl (..),
    pattern EApp,
    everything,
  )
import Prelude.Compat
import Protolude (mapMaybe)

-- TODO not recognising external self-module calls, which should not be generated right now, who are we anyway

-- TODO not recognising fns only used in top-lvl floated synthetic apps reachable from other code

removeUnusedFuns :: [(Atom, Int)] -> [Erl] -> [Erl]
removeUnusedFuns exps = loop
  where
    expsSet =
      S.fromList $
        mapMaybe
          ( \case
              (Atom _ name, n) -> Just (name, n)
              _ -> Nothing
          )
          exps

    loop :: [Erl] -> [Erl]
    loop asts = if changed then loop asts' else asts
      where
        used =
          expsSet
            <> foldMap
              ( everything
                  (<>)
                  ( \case
                      EFunRef (Atom Nothing name) n -> S.singleton (name, n)
                      EApp _ (EAtomLiteral (Atom Nothing name)) args -> S.singleton (name, length args)
                      _ -> S.empty
                  )
              )
              asts
        (Any changed, asts') = filterM (anyFalses . isInUsedSet used) asts

    isInUsedSet :: S.Set (Text, Int) -> Erl -> Bool
    isInUsedSet used = \case
      EFunctionDef _ _ (Atom Nothing name) vars _e ->
        (name, length vars) `S.member` used
      _ -> True

    anyFalses :: Bool -> (Any, Bool)
    anyFalses x = (Any (not x), x)
