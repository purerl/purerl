module Language.PureScript.Erl.CodeGen.Optimizer.Common where

import Control.Arrow (second)
import Control.Monad (when, (<=<))
import Control.Monad.State (MonadState (..), State, StateT, evalStateT, gets, modify, put, runState)
import Control.Monad.Supply.Class (MonadSupply (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Language.PureScript.Erl.CodeGen.AST
import Language.PureScript.Erl.CodeGen.Common (atomPS)
import Language.PureScript.PSString (PSString)
import Prelude.Compat

isFn :: (Text, Text) -> Erl -> Bool
isFn (moduleName, fnName) (EApp _ (EAtomLiteral (Atom (Just x) y)) []) =
  x == moduleName && y == fnName
isFn _ _ = False

isDict :: (Text, PSString) -> Erl -> Bool
isDict (moduleName, dictName) (EApp _ (EAtomLiteral (Atom (Just x) y)) []) = x == moduleName && y == atomPS dictName
isDict _ _ = False

isUncurriedFn :: (Text, PSString) -> Erl -> Bool
isUncurriedFn = isFnName

isFnName :: (Text, PSString) -> Erl -> Bool
isFnName (moduleName, dictName) (EAtomLiteral (Atom (Just x) y)) = x == moduleName && y == atomPS dictName
isFnName _ _ = False

isAppliedDict :: (Text, PSString) -> (Text, PSString) -> Erl -> Bool
isAppliedDict fnDict theDict (EApp _ (EApp _ fn []) [EApp _ dict []]) = isFnName theDict dict && isFnName fnDict fn
isAppliedDict fnDict theDict (EApp _ fn [EApp _ dict []]) = isFnName theDict dict && isFnName fnDict fn
isAppliedDict _ _ _ = False

isUncurriedFn' :: (Text, Text) -> Erl -> Bool
isUncurriedFn' (moduleName, fnName) (EAtomLiteral (Atom (Just x) y)) = x == moduleName && y == fnName
isUncurriedFn' _ _ = False

isCurriedFn :: (Text, PSString) -> Erl -> Bool
isCurriedFn = isDict

applyAll :: [a -> a] -> a -> a
applyAll = foldl1 (.)

applyAllM :: Monad m => [a -> m a] -> a -> m a
applyAllM = foldl1 (<=<)

-- Check if var x occurs in expression
occurs :: Text -> Erl -> Bool
occurs x =
  snd . flip runState False . everywhereOnErlTopDownM go
  where
    go :: Erl -> State Bool Erl
    go e@(EVar x') | x == x' = do
      put True
      pure e
    go e = pure e

-- Check if var x is (possibly) rebound in expression
isRebound :: Text -> Erl -> Bool
isRebound x =
  snd . flip runState False . everywhereOnErlTopDownM go
  where
    go :: Erl -> State Bool Erl
    -- go e@(EFunFull _ args) = do
    --   when (any matchBinder args) $ put True
    --   pure e
    go e@(EVarBind x' _) | x == x' = do
      put True
      pure e
    go e@(ECaseOf _ binds) = do
      when (any (matchCaseBinder . fst) binds) $ put True
      pure e
    go e = pure e

    -- matchBinder (EFunBinder es _, _) = any (occurs x) es

    matchCaseBinder (EBinder e) = occurs x e
    matchCaseBinder (EGuardedBinder e _) = occurs x e

-- TODO figure this into generic traversal with context pattern
replaceIdents :: [(Text, Erl)] -> Erl -> Erl
replaceIdents vars = go
  where
    -- Placeholder
    f :: Erl -> Erl
    f = id

    go :: Erl -> Erl
    go (EFunFull fname args) = f $ EFunFull fname $ map goFunHead args
    go (ECaseOf e binds) = f $ ECaseOf (go e) $ map goCase binds
    go (EUnary op e) = f $ EUnary op (go e)
    go (EBinary op e1 e2) = f $ EBinary op (go e1) (go e2)
    go (EFunctionDef t ssann a ss e) = f $ EFunctionDef t ssann a ss (go e)
    go (EVarBind x e) = f $ EVarBind x (go e)
    go (EApp meta e es) = f $ EApp meta (go e) (map go es)
    go (EBlock es) = f $ EBlock (map go es)
    go (ETupleLiteral es) = f $ ETupleLiteral (map go es)
    go (EMapLiteral binds) = f $ EMapLiteral $ map (second go) binds
    go (EMapPattern binds) = f $ EMapPattern $ map (second go) binds
    go (EMapUpdate e binds) = f $ EMapUpdate (go e) $ map (second go) binds
    go (EListLiteral es) = f $ EListLiteral (map go es)
    go (EListCons es e) = f $ EListCons (map go es) (go e)
    go v@(EVar var) = fromMaybe v $ lookup var vars
    go other = other

    -- -- Vars are *not* fresh inf case binders
    goCase :: (EBinder, Erl) -> (EBinder, Erl)
    goCase (EBinder e, e') = (EBinder (go e), go e')
    goCase (EGuardedBinder e (Guard eg), e') = (EGuardedBinder (go e) (Guard $ go eg), go e')

    goFunHead :: (EFunBinder, Erl) -> (EFunBinder, Erl)
    goFunHead (EFunBinder es g, e) = (EFunBinder es g', replaceIdents vars' e)
      where
        vars' = filter (\(var, _) -> not $ any (occurs var) es) vars
        g' = (\(Guard eg) -> Just $ Guard $ replaceIdents vars' eg) =<< g

-- Rename bound vars in preparation for hoisting expression into a parent scope when the expression may bind same variables as a sibling
-- Super restricted, only renames top level X = e bindings (possibly in a begin/end block) as this is what we generate
-- For general code would have to go down
renameBoundVars :: MonadSupply m => Erl -> m Erl
renameBoundVars = (`evalStateT` []) . go
  where
    go :: MonadSupply m => Erl -> StateT [(Text, Erl)] m Erl
    go (EVarBind x e) = do
      n <- fresh
      let x' = x <> "@" <> T.pack (show n)
      res <- gets (EVarBind x' . (`replaceIdents` e))
      modify ((x, EVar x') :)
      pure res
    go (EBlock es) = EBlock <$> traverse go es
    go e = gets (`replaceIdents` e)

collect :: Int -> Erl -> Erl
collect n e = go e []
  where
    go :: Erl -> [Erl] -> Erl
    go (EApp meta f x) acc | not (null x) =
      case x <> acc of
        args | length args >= n -> EApp meta f args
        args -> go f args
    go _other _acc = e
