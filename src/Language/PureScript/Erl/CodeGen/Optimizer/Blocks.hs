-- |
-- Optimizer steps for simplifying Javascript blocks
module Language.PureScript.Erl.CodeGen.Optimizer.Blocks
  ( collapseNestedBlocks,
  )
where

import Language.PureScript.Erl.CodeGen.AST
  ( Erl (EBlock),
    everywhereOnErl,
  )
import Prelude.Compat (concatMap)

-- |
-- Collapse blocks which appear nested directly below another block
collapseNestedBlocks :: Erl -> Erl
collapseNestedBlocks = everywhereOnErl collapse
  where
    collapse :: Erl -> Erl
    collapse (EBlock sts) =
      case concatMap go sts of
        [s] -> s
        sts' -> EBlock sts'
    collapse js = js
    go :: Erl -> [Erl]
    go (EBlock sts) = sts
    go s = [s]
