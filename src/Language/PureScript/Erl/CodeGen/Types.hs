module Language.PureScript.Erl.CodeGen.Types where

import Prelude
import Language.PureScript.Erl.CodeGen.AST
    ( Atom(..),
      EType(..) )
import Language.PureScript.Types
    ( SourceType,
      Type(ForAll, REmpty, RCons, KindApp, ConstrainedType, TypeVar,
           TypeConstructor, TypeApp) )
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State (State, runState, modify, get)
import qualified Language.PureScript.Environment as E
import Language.PureScript (ModuleName, Qualified (Qualified), ProperName (ProperName), nullSourceAnn, Ident, Environment, tyRecord)
import Debug.Trace (trace)
import qualified Language.PureScript as P
import qualified Data.Text as Text
import Control.Monad ( (<=<) )
import Language.PureScript.Erl.CodeGen.Common
    ( erlModuleNameBase, toAtomName, toVarName )
import qualified Data.Map as M
import Language.PureScript.Erl.Synonyms (replaceAllTypeSynonyms')
import Language.PureScript.Label (runLabel)
import Data.Bifunctor ( Bifunctor(bimap) )
import qualified Language.PureScript.Erl.CodeGen.Constants.PureScriptModules as EM
import Data.Either (fromRight)


uncurryType :: Int -> EType -> Maybe EType
uncurryType arity = uc []
  where
    uc [] uncurriedType@(TFun ts _) | length ts > 1 = Just uncurriedType
    uc ts (TFun [t1] t2) = uc (t1 : ts) t2
    uc ts t | length ts == arity = Just $ TFun (reverse ts) t
    uc _ _ = Nothing

type ETypeEnv = Map Text ([Text], EType)

translateType :: Environment -> SourceType -> (EType, ETypeEnv)
translateType env = flip runState Map.empty . go
  where
  go :: SourceType -> State ETypeEnv EType
  go = \case
    (TypeApp _ (TypeApp _ fn t1) t2)
      | fn == E.tyFunction ->
        TFun <$> sequence [go t1] <*> go t2
    ty | ty == E.tyInt -> pure TInteger
    ty | ty == E.tyNumber -> pure TFloat
    ty | ty == E.tyString -> pure $ TAlias (Atom Nothing "binary") []
    ty | ty == E.tyBoolean -> pure $ TAlias (Atom Nothing "boolean") []
    (TypeApp _ t1 t2)
      | t1 == E.tyArray ->
        TRemote "array" "array" . pure <$> go t2
    (TypeApp _ t1 t2)
      | t1 == ctorTy EM.erlDataList "List" ->
        TList <$> go t2
    (TypeApp _ (TypeApp _ t t1) t2) | t == ctorTy EM.erlDataMap "Map" -> do
      rT1 <- go t1
      rT2 <- go t2
      pure $ TMap $ Just [(rT1, rT2)]
    (TypeApp _ t1 t2)
      | t1 == ctorTy EM.effect "Effect" ->
        TFun [] <$> go t2
    (TypeApp _ tr t1)
      | tr == tyRecord ->
        let t1' = fromRight t1 $ replaceAllTypeSynonyms' (E.typeSynonyms env) (E.types env) t1
          in TMap <$> row t1' []
      where
        row (REmpty _) acc = pure $ Just acc
        row (RCons _ label t ttail) acc = do
          tt <- go t
          row ttail ((TAtom $ Just $ AtomPS Nothing $ runLabel label, tt) : acc)
        row (TypeVar _ _) _ = pure Nothing
        row (KindApp _ ty _) acc = row ty acc
        row t _ =
          trace ("Shouldn't find random type in row list: " <> show t) $ pure Nothing
    --
    (KindApp _ ty _) -> go ty
    (ForAll _ _ _ ty _) -> go ty
    (ConstrainedType _ _ ty) -> TFun [TAny] <$> go ty
    ty
      | Just (_, fnTypes) <- uncurriedFnTypes EM.dataFunctionUncurried "Fn" ty,
        tret <- last fnTypes,
        targs <- take (length fnTypes -1) fnTypes ->
        TFun <$> traverse go targs <*> go tret
    ty
      | Just (_, fnTypes) <- uncurriedFnTypes EM.effectUncurried "EffectFn" ty,
        tret <- last fnTypes,
        targs <- take (length fnTypes -1) fnTypes ->
        TFun <$> traverse go targs <*> go tret
    t@TypeApp {}
      | Just (tname, tys) <- collectTypeApp t ->
        do
          etys <- traverse go tys
          goTCtor etys tname
    (TypeVar _ var) -> pure $ TVar $ "_" <> toVarName var
    _ -> pure TAny

  goTCtor :: [EType] -> Qualified (ProperName 'P.TypeName) -> State ETypeEnv EType
  goTCtor tyargs = \case
    tname
      | Just (args, t) <- M.lookup tname (E.typeSynonyms env),
        length args == length tyargs,
        (Qualified _mn _ident) <- tname -> do
        let erlName = erlTypeName tname
        get
          >>= ( \case
                  Just _ ->
                    pure ()
                  Nothing -> do
                    -- dummy to prevent recursing
                    modify (M.insert erlName (map (("_" <>) . toVarName . fst) args, TAny))
                    tt <- go t
                    modify (M.insert erlName (map (("_" <>) . toVarName . fst) args, tt))
              )
            . M.lookup erlName
        pure $ TAlias (Atom Nothing erlName) tyargs
    tname
      | Just (_, t) <- M.lookup tname (E.types env),
        --  DataType [(Text, Maybe SourceKind)] [(ProperName 'ConstructorName, [SourceType])]
        P.DataType _dt dtargs (ctors :: [(ProperName 'P.ConstructorName, [SourceType])]) <- t,
        length dtargs == length tyargs,
        -- can't ignore mn for external stuff
        (Qualified mn' _ident) <- tname ->
        do
          let erlName = erlTypeName tname
          get
            >>= ( \case
                    Just _ -> pure ()
                    Nothing -> do
                      -- dummy to prevent recursing
                      modify (M.insert erlName (map (("_" <>) . toVarName . (\(x, _, _) -> x)) dtargs, TAny))
                      let alt (ctorName, ctorArgs) = do
                            targs <- traverse go ctorArgs
                            pure $ case isNewtypeConstructor env (Qualified mn' ctorName) of
                              Just True -> head targs
                              Just False -> TTuple (TAtom (Just $ Atom Nothing $ toAtomName $ P.runProperName ctorName) : targs)
                              Nothing ->
                                -- TODO if we put this in the defining module maybe an opaque type would be useful to prevent some misuse
                                TAny
                      tt <- traverse alt ctors
                      let union = case length tt of
                            0 -> TAny -- arguably none()
                            _ -> TUnion tt
                      modify (M.insert erlName (map (("_" <>) . toVarName . (\(x, _, _) -> x)) dtargs, union))
                )
              . M.lookup erlName

          pure $ TAlias (Atom Nothing erlName) tyargs
    -- not a data type/alias we can find or not fully applied
    _ -> pure TAny

  ctorTy :: ModuleName -> Text -> SourceType
  ctorTy modName fn = TypeConstructor nullSourceAnn (Qualified (P.ByModuleName modName) (ProperName fn))

uncurriedFnTypes :: ModuleName -> Text -> SourceType -> Maybe (Int, [SourceType])
uncurriedFnTypes moduleName fnName = check <=< collectTypeApp
  where
  check :: (Qualified (ProperName 'P.TypeName), [SourceType]) -> Maybe (Int, [SourceType])
  check (Qualified (P.ByModuleName mn) (ProperName fnN), tys) =
    let n = length tys - 1
      in if n >= 1 && n <= 10 && fnN == (fnName <> Text.pack (show n)) && mn == moduleName
          then Just (n, tys)
          else Nothing
  check _ = Nothing

collectTypeApp :: SourceType -> Maybe (Qualified (ProperName 'P.TypeName), [SourceType])
collectTypeApp = go []
  where
    go acc (TypeConstructor _ tc) = Just (tc, acc)
    go acc (TypeApp _ t1 t2) = go (t2 : acc) t1
    go acc (ForAll _ _ _ ty _) = go acc ty
    go _ _ = Nothing


-- substitute any() for any var X, for use in specs
replaceVars :: EType -> EType
replaceVars = go
  where
  go = \case
    TVar _ -> TAny
    TFun args res -> TFun (go <$> args) (go res)
    TList arg -> TList $ go arg
    TMap (Just args) -> TMap $ Just $ map (bimap go go) args
    TTuple args -> TTuple $ go <$> args
    TUnion args -> TUnion $ go <$> args
    TRemote a b args -> TRemote a b $ go <$> args
    TAlias x args -> TAlias x $ go <$> args
    z -> z


erlTypeName :: Qualified (ProperName 'P.TypeName) -> Text
erlTypeName (Qualified mn' ident)
  | P.ByModuleName mn'' <- mn' =
    -- TODO this is easier to read but clashes builtins
    -- , mn'' /= mn =
    toAtomName $ erlModuleNameBase mn'' <> "_" <> P.runProperName ident
  | otherwise =
    toAtomName $ P.runProperName ident

isNewtypeConstructor :: Environment -> Qualified (P.ProperName 'P.ConstructorName) -> Maybe Bool
isNewtypeConstructor e ctor = case lookupConstructor e ctor of
  Just (P.Newtype, _, _, _) -> Just True
  Just (P.Data, _, _, _) -> Just False
  Nothing -> Nothing

  where
  lookupConstructor :: Environment -> Qualified (P.ProperName 'P.ConstructorName) -> Maybe (P.DataDeclType, ProperName 'P.TypeName, P.SourceType, [Ident])
  lookupConstructor env' c =
    c `M.lookup` P.dataConstructors env'
