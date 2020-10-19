module Language.PureScript.Erl.CoreFn 
( transformCoreFn
)
where

import Protolude hiding (Type)

import Language.PureScript.AST.SourcePos
import Language.PureScript.AST.Literals

import Language.PureScript.CoreFn.Ann
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Module
import Language.PureScript.CoreFn.Traversals
import Language.PureScript.PSString
import Language.PureScript.Names

transformCoreFn :: Module Ann -> Module Ann
transformCoreFn m = m {moduleDecls = transformModuleDecls $ moduleDecls m}

transformModuleDecls :: [Bind Ann] -> [Bind Ann]
transformModuleDecls = map transformBinds
  where
  (transformBinds, _, _) = everywhereOnValues identity transformExprs identity
  transformExprs = restoreClosedRecordUpdate

-- Reverse (most of?) the applications of purs optimizeClosedRecordUpdate
restoreClosedRecordUpdate :: Expr Ann -> Expr Ann
restoreClosedRecordUpdate = \case 
  expr@(Literal a (ObjectLiteral fields)) -> 
    case go (Nothing, []) fields of
      Just (var, fields') -> 
        ObjectUpdate a (Var (nullSourceSpan, [], Nothing, Nothing) var) fields'
      Nothing -> expr
  other -> other

  where
  go :: (Maybe (Qualified Ident), [(PSString, Expr a)]) -> [(PSString, Expr a)] -> Maybe (Qualified Ident, [(PSString, Expr a)])
  go (Nothing, updates) ((l, Accessor _ l' (Var _ var)):fields) | l == l' =
    go (Just var, updates) fields
  go acc@(Just var, _) ((l, Accessor _ l' (Var _ var')):fields) | l == l' && var == var' =
    go acc fields
  go _ ((_, Accessor _ _ _):_) =
    Nothing
  go (var, updates) (update:fields) = go (var, update:updates) fields
  go (Just v, acc) [] = Just (v, acc)
  go _ [] = Nothing
