{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- This module generates code in the simplified Erlang intermediate representation from Purescript code
module Language.PureScript.Erl.CodeGen
  ( module AST,
    moduleToErl,
    buildCodegenEnvironment,
    CodegenEnvironment,
  )
where

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Control.Monad (foldM, replicateM, unless)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Supply.Class (MonadSupply (fresh))
import Control.Monad.Writer (MonadWriter (..), Any (Any), WriterT (runWriterT))
import Data.Either (fromRight)
import Data.Foldable (find, traverse_, foldl')
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable (forM)
import Debug.Trace (traceM)
import qualified Language.PureScript as P
import Language.PureScript.AST (SourceSpan, nullSourceSpan)
import qualified Language.PureScript.Constants.Prelude as C
import qualified Language.PureScript.Constants.Prim as C
import Language.PureScript.CoreFn
  ( Ann,
    Bind (..),
    Binder (..),
    CaseAlternative (CaseAlternative, caseAlternativeBinders),
    Expr (..),
    Literal (..),
    Meta (IsConstructor, IsNewtype, IsTypeClassConstructor, IsSyntheticApp),
    Module (Module),
    everywhereOnValues,
    extractAnn,
    ssAnn,
  )
import Language.PureScript.Environment as E
  ( Environment (names, typeSynonyms, types),
    tyFunction,
  )
import Language.PureScript.Erl.CodeGen.AST as AST
import Language.PureScript.Erl.CodeGen.CheckedWrapper (typecheckWrapper)
import Language.PureScript.Erl.CodeGen.Common
  ( ModuleType (ForeignModule, PureScriptModule),
    atomModuleName,
    freshNameErl,
    freshNameErl',
    identToVar,
    toAtomName, identToAtomName, runIdent'
  )
import Language.PureScript.Erl.CodeGen.Constants.PureScriptModules
  ( dataFunctionUncurried,
    effectUncurried,
  )
import Language.PureScript.Erl.Errors (MultipleErrors, addHint, errorMessage, rethrow, rethrowWithPosition)
import Language.PureScript.Erl.Errors.Types
  ( SimpleErrorMessage
      ( InvalidFFIArity,
        MissingFFIImplementations,
        UnusedFFIImplementations
      ),
  )
import Language.PureScript.Erl.Synonyms (replaceAllTypeSynonyms')
import Language.PureScript.Errors (ErrorMessageHint (..))
import Language.PureScript.Names
  ( Ident (Ident, UnusedIdent, InternalIdent),
    ModuleName (..),
    ProperName (ProperName),
    Qualified (..),
    InternalIdentData (RuntimeLazyFactory, Lazy)
  )
import Language.PureScript.Options (Options)
import Language.PureScript.Traversals (sndM)
import Language.PureScript.Types
  ( SourceType,
    Type (..),
  )
import Prelude.Compat
import Language.PureScript.Erl.CodeGen.Types (ETypeEnv, uncurriedFnTypes, replaceVars, translateType, uncurryType)
import Language.PureScript.CoreFn.Laziness (applyLazinessTransform)
import Language.PureScript (internalError)

identToTypeclassCtor :: Ident -> Atom
identToTypeclassCtor a = Atom Nothing (runIdent' a)

qualifiedToErl' :: ModuleName -> ModuleType -> Ident -> Atom
qualifiedToErl' mn' moduleType ident = Atom (Just $ atomModuleName mn' moduleType) (runIdent' ident)

-- Top level definitions are everywhere fully qualified, variables are not.
qualifiedToErl :: ModuleName -> Qualified Ident -> Atom
qualifiedToErl mn (Qualified (P.ByModuleName mn') ident)
  -- Local reference to local non-exported function
  | mn == mn' -- TODO making all local calls non qualified - for memoization onload - revert
  -- && ident `Set.notMember` declaredExportsSet  =
    =
    Atom Nothing (runIdent' ident)
  -- Reference other modules or exported things via module name
  | otherwise =
    qualifiedToErl' mn' PureScriptModule ident
qualifiedToErl _ (Qualified (P.BySourcePos _) ident) = Atom Nothing (runIdent' ident)




uncurriedFnArity :: ModuleName -> T.Text -> SourceType -> Maybe Int
uncurriedFnArity moduleName fnName ty = fst <$> uncurriedFnTypes moduleName fnName ty

concatRes :: [([a], [b], ETypeEnv)] -> ([a], [b], ETypeEnv)
concatRes x = (concatMap (\(a, _, _) -> a) x, concatMap (\(_, b, _) -> b) x, M.unions $ (\(_, _, c) -> c) <$> x)

isFullySaturatedForeignCall :: ModuleName -> M.Map (Qualified Ident) Int -> Expr Ann -> [a] -> Bool
isFullySaturatedForeignCall mn actualForeignArities var args = case var of
  Var _ qi
    | P.isQualifiedWith mn qi,
      Just arity <- M.lookup qi actualForeignArities,
      length args == arity ->
      True
  _ -> False


data FnArity = EffFnXArity Int | FnXArity Int | Arity (Int, Int)
  deriving (Eq, Show)



data CodegenEnvironment = CodegenEnvironment E.Environment (M.Map (Qualified Ident) FnArity)

buildCodegenEnvironment :: E.Environment -> CodegenEnvironment
buildCodegenEnvironment env = CodegenEnvironment env explicitArities
  where
    tyArity :: SourceType -> FnArity
    tyArity t = Arity $ go 0 t'
      where
        t' = fromRight t $ replaceAllTypeSynonyms' (E.typeSynonyms env) (E.types env) t

        go n = \case
          ConstrainedType _ _ ty -> go (n + 1) ty
          ForAll _ _ _ ty _ -> go n ty
          other -> (n, go' other)
        go' = \case
          TypeApp _ (TypeApp _ fn _) ty | fn == E.tyFunction -> 1 + go' ty
          ForAll _ _ _ ty _ -> go' ty
          _ -> 0

    explicitArities :: M.Map (Qualified Ident) FnArity
    explicitArities = tyArity <$> types

    types :: M.Map (Qualified Ident) SourceType
    types = M.map (\(t, _, _) -> t) $ E.names env

data Arities = Arities
  { _effective :: M.Map (Qualified Ident) FnArity
  , _used :: M.Map (Qualified Ident) (Set Int)
  , _actualForeign :: M.Map (Qualified Ident) Int
  }

findArities :: ModuleName -> [Bind Ann] -> CodegenEnvironment -> [(T.Text, Int)] -> Arities
findArities mn decls (CodegenEnvironment _ explicitArities) foreignExports = Arities arities usedArities actualForeignArities
  where
  actualForeignArities :: M.Map (Qualified Ident) Int
  actualForeignArities = M.fromList $ map (\(x, n) -> (Qualified (P.ByModuleName mn) (Ident x), n)) foreignExports

  arities :: M.Map (Qualified Ident) FnArity
  arities =
    -- max arities is max of actual impl and most saturated application
    let inferredArities = foldr findUsages (Set.singleton <$> actualForeignArities) decls
        inferredMaxArities = M.mapMaybe (fmap (\n -> Arity (0, n)) . Set.lookupMax) inferredArities
      in explicitArities `M.union` inferredMaxArities

  usedArities :: M.Map (Qualified Ident) (Set Int)
  usedArities = foldr findUsages M.empty decls

  findUsages :: Bind Ann -> M.Map (Qualified Ident) (Set Int) -> M.Map (Qualified Ident) (Set Int)
  findUsages (NonRec _ _ val) apps = findUsages' val apps
  findUsages (Rec vals) apps = foldr (findUsages' . snd) apps vals

  findUsages' :: Expr Ann -> M.Map (Qualified Ident) (Set Int) -> M.Map (Qualified Ident) (Set Int)
  findUsages' expr apps = case expr of
    e@App {} ->
      let (f, args) = unApp e []
          apps' = foldr findUsages' apps args
        in case f of
            Var (_, _, _, Just IsNewtype) _ -> apps'
            -- This is an app but we inline these
            Var (_, _, _, Just (IsConstructor _ fields)) (Qualified _ _)
              | length args == length fields ->
                apps'
            Var (_, _, _, Just IsTypeClassConstructor) _ ->
              apps'
            -- Don't count fully saturated foreign import call, it will be called directly
            Var _ _
              | isFullySaturatedForeignCall mn actualForeignArities f args ->
                apps'
            Var _ (Qualified q ident)
              | thisModule q ->
                M.insertWith Set.union (Qualified (P.ByModuleName mn) ident) (Set.singleton $ length args) apps'
            _ -> findUsages' f apps'
    v@Var {} ->
      case v of
        -- Must actually not assume this is 0 as it may be a ref fn/1 or fnx/efffnx/n
        -- Should record separately - or assume that 0 may mean non-0 for this reason?
        Var _ (Qualified q ident)
          | thisModule q ->
            M.insertWith Set.union (Qualified (P.ByModuleName mn) ident) (Set.singleton 0) apps
        _ -> apps
    Accessor _ _ e -> findUsages' e apps
    ObjectUpdate _ e es -> findUsages' e $ foldr (findUsages' . snd) apps es
    Abs _ _ e -> findUsages' e apps
    Case _ e es -> foldr findUsages' (foldr findUsagesCase apps es) e
    Let _ b e' ->
      findUsages' e' $ foldr findUsages'' apps b
    Literal _ litExpr -> case litExpr of
      NumericLiteral _ -> apps
      StringLiteral _ -> apps
      CharLiteral _ -> apps
      BooleanLiteral _ -> apps
      ArrayLiteral exprs -> foldr findUsages' apps exprs
      ObjectLiteral fields -> foldr (findUsages' . snd) apps fields
    Constructor {} -> apps
    where
      unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
      unApp (App _ val arg) args = unApp val (arg : args)
      unApp other args = (other, args)

      thisModule (P.ByModuleName mn') = mn' == mn
      thisModule (P.BySourcePos _) = True

  findUsages'' (NonRec _ _ e) apps = findUsages' e apps
  findUsages'' (Rec binds) apps = foldr (findUsages' . snd) apps binds

  findUsagesCase (CaseAlternative _ (Right e)) apps = findUsages' e apps
  findUsagesCase (CaseAlternative _ (Left ges)) apps = foldr findUsages' apps $ concatMap (\(a, b) -> [a, b]) ges


-- |
-- Generate code in the simplified Erlang intermediate representation for all declarations in a
-- module.
moduleToErl ::
  forall m.
  (Monad m, MonadReader Options m, MonadSupply m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  CodegenEnvironment ->
  Module Ann ->
  [(T.Text, Int)] ->
  m ([(Atom, Int)], [Erl], [Erl], [Erl], [(Atom, Int)], [Erl], Map Atom Int)
moduleToErl codegenEnv m@(Module _ _ mn _ _ _ _ _ _) foreignExports =
  rethrow (addHint (ErrorInModule mn)) $ do
    (res, (warnings, Any needRuntimeLazy)) <- runWriterT $ moduleToErl' codegenEnv m foreignExports
    tell warnings

    pure $ if needRuntimeLazy then
      let (exports, namedSpecs, foreignSpecs, decls, safeExports, safeDecls, memoizable) = res

      in (exports, namedSpecs, foreignSpecs, runtimeLazy : runtimeLazyCurried : decls, safeExports, safeDecls, memoizable)
    else
      res

  where

  -- Lazy initialisation runtime - we almost don't need this at all, except in the case of an insufficiently lazy recursive reference,
  -- we would naturally recurse forever instead of throwing as specced. Given most instances don't require this, the overhead may be worth
  -- finding an explicit error instead of hang
  runtimeLazy :: Erl
  runtimeLazy = EFunctionDef Nothing Nothing (Atom Nothing $ identToAtomName $ InternalIdent RuntimeLazyFactory) ["CtxRef", "Name", "ModuleName", "Init"] runtimeLazyBody

  -- TODO I don't want to need this
  runtimeLazyCurried :: Erl
  runtimeLazyCurried = EFunctionDef Nothing Nothing (Atom Nothing $ identToAtomName $ InternalIdent RuntimeLazyFactory) [ "CtxRef" ] $
    EFunFull Nothing [(EFunBinder [EVar "Name", EVar "ModuleName", EVar "Init"] Nothing, runtimeLazyBody)]

  runtimeLazyBody :: Erl
  runtimeLazyBody =
    EBlock
      [
        -- TODO This means this is never cached at the top level. In fact it might simply not work
        EVarBind "StateKey" (ETupleLiteral [ EVar "Name", EVar "ModuleName", EAtomLiteral (Atom Nothing "lazy_state_@purerl"), EVar "CtxRef" ])
      , EVarBind "ValueKey" (ETupleLiteral [ EVar "Name", EVar "ModuleName", EAtomLiteral (Atom Nothing "lazy_value_@purerl"), EVar "CtxRef" ])
      , EFun1 Nothing "LineNo"
         ( EBlock
          [ ECaseOf (qualFunCall "erlang" "get" [EVar "StateKey"])
            [ ( EBinder $ litAtom "initialized", qualFunCall "erlang" "get" [EVar "ValueKey"] )
            , ( EBinder $ litAtom "initializing", qualFunCall "erlang" "throw" [ETupleLiteral [ litAtom "not_finished_initializing", EVar "Name", EVar "ModuleName", EVar "LineNo" ] ] )
            , ( EBinder $ litAtom "undefined", EBlock
                [ qualFunCall "erlang" "put" [ EVar "StateKey", litAtom "initializing" ]
                , EVarBind "Value" (EApp RegularApp (EVar "Init") [ litAtom "unit" ])
                , qualFunCall "erlang" "put" [ EVar "ValueKey", EVar "Value" ]
                , qualFunCall "erlang" "put" [ EVar "StateKey", litAtom "initialized" ]
                , EVar "Value"
                ]
              )
            ]
          ]
         )
      ]

moduleToErl' ::
  forall m.
  (Monad m, MonadReader Options m, MonadSupply m, MonadError MultipleErrors m, MonadWriter (MultipleErrors, Any) m) =>
  CodegenEnvironment ->
  Module Ann ->
  [(T.Text, Int)] ->
  m ([(Atom, Int)], [Erl], [Erl], [Erl], [(Atom, Int)], [Erl], Map Atom Int)
moduleToErl' cgEnv@(CodegenEnvironment env explicitArities) (Module _ _ mn _ _ declaredExports _ foreigns origDecls) foreignExports =
  do
    res <- traverse topBindToErl decls
    reexports <- traverse reExportForeign foreigns
    let exportTypes = mapMaybe (\(_, _, t, _) -> t) reexports
        foreignSpecs = map (\(ident, ty) -> ESpec (qualifiedToErl' mn ForeignModule ident) (replaceVars ty)) exportTypes

        (exports, erlDecls, typeEnv) = concatRes $ res <> map (\(a, b, _, d) -> (a, b, d)) reexports
        namedSpecs = map (\(name, (args, ty)) -> EType (Atom Nothing name) args ty) $ M.toList typeEnv

    traverse_ checkExport foreigns
    let usedFfi = Set.fromList $ map runIdent' foreigns
        definedFfi = Set.fromList (map fst foreignExports)
        unusedFfi = definedFfi Set.\\ usedFfi
    unless (Set.null unusedFfi) $
      tell (errorMessage $ UnusedFFIImplementations mn (Ident <$> Set.toAscList unusedFfi), mempty)

    let attributes = findAttributes decls

    safeDecls <- concat <$> traverse (typecheckWrapper mn) erlDecls


    let fnl (EFunctionDef _ _ fnName args _) = Just (fnName, length args)
        fnl _ = Nothing
        safeExports = mapMaybe fnl safeDecls

        memoizable =
          M.mapKeys (qualifiedToErl mn) $
            M.mapMaybe
              ( \case
                  Arity (n, _) | n > 0 -> Just n
                  _ -> Nothing
              )
              arities
              -- Var _ qi@(Qualified _ _)
    return (exports, namedSpecs, foreignSpecs, attributes ++ erlDecls, safeExports, safeDecls, memoizable)
  where
    declaredExportsSet = Set.fromList declaredExports

    Arities arities usedArities actualForeignArities = findArities mn decls cgEnv foreignExports

    decls :: [Bind Ann]
    decls = go <$> origDecls
      where
        go (NonRec ann ident val) = NonRec ann ident (removeDollars val)
        go (Rec vals) = Rec $ map (second removeDollars) vals

        removeDollars = fe
          where
            (_, fe, _) = everywhereOnValues id go id

            go (App ann (App _ (Var _ apply) f) a)
              | apply == Qualified (P.ByModuleName dataFunction) (Ident C.apply) =
                App ann f a
            go (App ann (App _ (Var _ apply) a) f)
              | apply == Qualified (P.ByModuleName dataFunction) (Ident C.applyFlipped) =
                App ann f a
            go other = other

            dataFunction = ModuleName "Data.Function"

    types :: M.Map (Qualified Ident) SourceType
    types = M.map (\(t, _, _) -> t) $ E.names env

    findAttributes :: [Bind Ann] -> [Erl]
    findAttributes expr = map (uncurry EAttribute) $ mapMaybe getAttribute $ concatMap onBind expr
      where
        getAttribute (TypeApp _ (TypeApp _ (TypeConstructor _ (Qualified (P.ByModuleName _) (ProperName "Attribute"))) (TypeLevelString _ a)) (TypeLevelString _ b)) =
          Just (a, b)
        getAttribute _ = Nothing

        getType ident = M.lookup (Qualified (P.ByModuleName mn) ident) types

        onRecBind ((_, ident), _) = getType ident
        onBind (NonRec _ ident _) = catMaybes [getType ident]
        onBind (Rec vals) = mapMaybe onRecBind vals



    -- 're-export' foreign imports in the @ps module - also used for internal calls for non-exported foreign imports
    reExportForeign :: Ident -> m ([(Atom, Int)], [Erl], Maybe (Ident, EType), ETypeEnv)
    reExportForeign ident = do
      let arity = exportArity ident
          fullArity = case M.lookup (Qualified (P.ByModuleName mn) ident) arities of
            Just (Arity (0, n)) -> n
            _ -> arity
          wrapTy (ty', tenv) = case arity of
            0 -> Just (TFun [] ty', tenv)
            _ -> (,tenv) <$> uncurryType arity ty'

          ffiTyEnv = wrapTy . translateType env =<< M.lookup (Qualified (P.ByModuleName mn) ident) types

      args <- replicateM fullArity freshNameErl
      let body = EApp RegularApp (EAtomLiteral $ qualifiedToErl' mn ForeignModule ident) (take arity $ map EVar args)
          body' = curriedApp (drop arity $ map EVar args) body
          fun = curriedLambda body' args
      fident <- fmap (Ident . ("f" <>) . T.pack . show) fresh
      let var = Qualified P.ByNullSourcePos fident
          wrap e = EBlock [EVarBind (identToVar fident) fun, e]
      (idents, erl, env) <- generateFunctionOverloads Nothing True Nothing (ssAnn nullSourceSpan) ident (Atom Nothing $ runIdent' ident) (Var (ssAnn nullSourceSpan) var) wrap
      let combinedTEnv = M.union env (maybe M.empty snd ffiTyEnv)
      pure (idents, erl, (ident,) . fst <$> ffiTyEnv, combinedTEnv)



    exportArity :: Ident -> Int
    exportArity ident = fromMaybe 0 $ findExport $ runIdent' ident

    checkExport :: Ident -> m ()
    checkExport ident =
      case (findExport (runIdent' ident), M.lookup (Qualified (P.ByModuleName mn) ident) explicitArities) of
        -- TODO is it meaningful to check against inferred arities (as we are just now) or only explicit ones
        -- This probably depends on the current codegen

        -- If we know the foreign import type (because it was exported) and the actual FFI type, it is an error if
        -- the actual implementation has higher arity than the type does
        -- If the actual implementation has lower arity, it may be just returning a function
        (Just m, Just (Arity (nc, n)))
          | m > nc + n ->
            throwError . errorMessage $ InvalidFFIArity mn (runIdent' ident) m (nc + n)
        -- If we don't know the declared type of the foreign import (because it is not exported), then we cannot say
        -- what the implementation's arity should be, as it may be higher than can be inferred from applications in this
        -- module (because there is no fully saturated application) or lower (because the ffi returns a function)
        (Just _, Nothing) ->
          pure ()
        -- We certainly can tell if an import exists and the implementation isn't found
        -- The opposite situation is handled at the top level of moduleToErl
        (Nothing, _) ->
          throwError . errorMessage $ MissingFFIImplementations mn [ident]
        _ -> pure ()

    findExport :: T.Text -> Maybe Int
    findExport n = snd <$> find ((n ==) . fst) foreignExports

    isTopLevelBinding :: Qualified Ident -> Bool
    isTopLevelBinding (Qualified (P.ByModuleName _) _) = True
    isTopLevelBinding (Qualified (P.BySourcePos _) (InternalIdent (Lazy ident))) = Ident ident `elem` topLevelNames
    isTopLevelBinding (Qualified (P.BySourcePos _) (InternalIdent RuntimeLazyFactory)) = True
    isTopLevelBinding (Qualified (P.BySourcePos _) ident) = ident `elem` topLevelNames

    topLevelNames = concatMap topLevelName decls
      where
        topLevelName :: Bind Ann -> [Ident]
        topLevelName (NonRec _ ident _) = [ident]
        topLevelName (Rec vals) = map (snd . fst) vals

    uncurriedFnArity' :: (Int -> FnArity) -> ModuleName -> T.Text -> Qualified Ident -> Maybe FnArity
    uncurriedFnArity' ctor fnMod fn ident =
      case M.lookup ident types of
        Just t -> ctor <$> uncurriedFnArity fnMod fn t
        _ -> Nothing

    effFnArity = uncurriedFnArity' EffFnXArity effectUncurried "EffectFn"
    fnArity = uncurriedFnArity' FnXArity dataFunctionUncurried "Fn"

    topBindToErl :: (Bind Ann -> m ([(Atom, Int)], [Erl], ETypeEnv))
    topBindToErl = \case
      NonRec ann ident val -> topNonRecToErl False ann ident val
      Rec vals ->
        let (vals', needRuntimeLazy@(Any needLazyRef)) = applyLazinessTransform mn vals
        in
          concatRes <$>
          (writer (vals', (mempty, needRuntimeLazy)) >>=
              traverse (uncurry . uncurry $ topNonRecToErl needLazyRef))


    topNonRecToErl :: Bool -> Ann -> Ident -> Expr Ann -> m ([(Atom, Int)], [Erl], ETypeEnv)
    topNonRecToErl inLazyRecGroup (ss, _, _, _) ident val = do
      let eann@(_, _, _, meta') = extractAnn val
          ident' = case meta' of
            Just IsTypeClassConstructor -> identToTypeclassCtor ident
            _ -> Atom Nothing $ runIdent' ident

      val' <- ensureFreshVars_ val
      (maybeVarName, wrapper) <- if inLazyRecGroup then
        do
          lazyVarName <- freshNameErl' "LazyCtxRef"
          pure (Just lazyVarName, \e -> EBlock [ EVarBind lazyVarName $ litAtom "top_level", e ])
        else
          pure (Nothing, id)
      generateFunctionOverloads maybeVarName False (Just ss) eann ident ident' val' wrapper

    generateFunctionOverloads :: Maybe T.Text -> Bool -> Maybe SourceSpan -> Ann -> Ident -> Atom -> Expr Ann -> (Erl -> Erl) -> m ([(Atom, Int)], [Erl], ETypeEnv)
    generateFunctionOverloads lazyVarName isForeign ss eann ident ident' val outerWrapper = do
      -- Always generate the plain curried form, f x y = ... -~~> f() -> fun (X) -> fun (Y) -> ... end end.
      let qident = Qualified (P.ByModuleName mn) ident
          replaceLazyCall = everywhereOnErl go
            where
              go (EApp RegularApp lazyFactory [ ])
               | lazyFactory == EAtomLiteral (Atom Nothing $ identToAtomName $ InternalIdent RuntimeLazyFactory)
               , Just varName <- lazyVarName
               = EApp RegularApp lazyFactory [ EVar varName ]
              go e = e

      erl <- replaceLazyCall <$> valueToErl val

      let translated = translateType env <$> M.lookup qident types
          erlangType = replaceVars . fst <$> translated
          etypeEnv = maybe M.empty snd translated

      let curried = ([(ident', 0)], [EFunctionDef (TFun [] <$> erlangType) ss ident' [] (outerWrapper erl)])

      -- For effective > 0 (either plain curried funs, FnX or EffectFnX) generate an uncurried overload
      -- f x y = ... -~~> f(X,Y) -> ((...)(X))(Y).
      -- Relying on inlining to clean up some junk here
      let mkRunApp modName prefix n = App eann (Var eann (Qualified (P.ByModuleName modName) (Ident $ prefix <> T.pack (show n))))
          applyStep fn a = App eann fn (Var eann (Qualified P.ByNullSourcePos (Ident a)))

          countAbs :: Expr Ann -> Int
          countAbs (Abs _ _ e) = 1 + countAbs e
          countAbs _ = 0

      let curriedWrappingUncurried arity = do
            vars <- replicateM arity freshNameErl
            let app = EApp RegularApp (EAtomLiteral ident') (EVar <$> vars)
                callUncurriedErl = curriedLambda app vars
            pure
              ([(ident', 0)], [EFunctionDef (TFun [] <$> erlangType) ss ident' [] callUncurriedErl])
      let uncurriedWrappingCurried arity = do
            vars <- replicateM arity freshNameErl
            let callCurriedErl = curriedApp (EVar <$> vars) $ EApp RegularApp (EAtomLiteral ident') []
            pure
              ([(ident', arity)], [EFunctionDef (uncurryType arity =<< erlangType) ss ident' vars callCurriedErl])

      let guard :: Monoid a => Bool -> a -> a
          guard t x = if t then x else mempty

      let checkUsed :: (Set Int -> Bool) -> Bool
          checkUsed f =
            ident `Set.member` declaredExportsSet || isForeign
              || maybe False f (M.lookup qident usedArities)
              || isLazy ident
          isLazy (InternalIdent (Lazy _)) = True
          isLazy (InternalIdent RuntimeLazyFactory) = True
          isLazy _ = False

          lazyArity (InternalIdent (Lazy _)) = Just (Arity (0, 1))
          lazyArity (InternalIdent RuntimeLazyFactory) = Just (Arity (0, 3))
          lazyArity _ = Nothing

          usedArity a = checkUsed (Set.member a)
          usedAnyArity = checkUsed (not . Set.null)
          usedExceptArity a = checkUsed (not . Set.null . Set.delete a)

      -- Apply in CoreFn then translate to take advantage of translation of full/partial application
      (res1, res2) <- case effFnArity qident <|> fnArity qident <|> M.lookup qident arities <|> lazyArity ident of
        Just (EffFnXArity arity) -> do
          vars <- replicateM arity freshNameErl
          erl' <- valueToErl $ foldl applyStep (mkRunApp effectUncurried C.runEffectFn arity val) vars
          pure $ curried <> ([(ident', arity)], [EFunctionDef (uncurryType arity =<< erlangType) ss ident' vars (outerWrapper (EApp RegularApp erl' []))])
        Just (FnXArity arity) -> do
          -- Same as above
          vars <- replicateM arity freshNameErl
          erl' <- valueToErl $ foldl applyStep (mkRunApp dataFunctionUncurried C.runFn arity val) vars
          pure $ curried <> ([(ident', arity)], [EFunctionDef (uncurryType arity =<< erlangType) ss ident' vars (outerWrapper erl')])
        Just (Arity (n, m)) | n + m > 0 ->
          do
            let arity = n + m

            -- experimental split between typeclass & regular arguments
            -- TODO this still duplicates code
            vars <- replicateM arity freshNameErl
            split <-
              if n == 0 || m == 0
                then pure ([], [])
                else do
                  erl'' <- valueToErl $ foldl applyStep val (take n vars)
                  pure ([(ident', n)], [EFunctionDef Nothing ss ident' (take n vars) (outerWrapper erl'')])

            if countAbs val == arity && usedArity arity
              then do
                erl' <- valueToErl $ foldl applyStep val vars
                curriedWrap <- curriedWrappingUncurried arity
                let uncurried = ([(ident', arity)], [EFunctionDef (uncurryType arity =<< erlangType) ss ident' vars (outerWrapper erl')])
                pure $ uncurried <> guard (usedExceptArity arity) curriedWrap <> split
              else
                if usedAnyArity
                  then do
                    uncurriedWrap <- uncurriedWrappingCurried arity
                    pure $ curried <> guard (usedArity arity) uncurriedWrap <> split
                  else do
                    pure ([], [])
        _ ->
          if usedAnyArity
            then pure curried
            else pure mempty
      pure $
        if ident `Set.member` declaredExportsSet
          then (res1, res2, etypeEnv)
          else ([], res2, etypeEnv)

    bindToErl :: Bind Ann -> m [Erl]
    bindToErl (NonRec _ ident val) =
      pure . EVarBind (identToVar ident) <$> valueToErl' (Just ident) val
    -- For recursive bindings F(X) = E1, G(X) = E2, ... we have a problem as the variables are not
    -- in scope until each expression is defined. To avoid lifting to the top level first generate
    -- funs which take a tuple of such funs F'({F', G'}) -> (X) -> E1 etc.
    -- with occurences of F, G replaced in E1, E2 with F'({F',G'})
    -- and then bind these F = F'({F',G'})
    -- TODO: Only do this if there are multiple mutually recursive bindings! Else a named fun works.
    bindToErl (Rec origVals) = do
      let (vals, needRuntimeLazy@(Any needLazyRef)) = applyLazinessTransform mn origVals
      tell (mempty, needRuntimeLazy)
      lazyVarName <- freshNameErl' "LazyCtxRef"
      let vars = identToVar . snd . fst <$> vals
          varTup = ETupleLiteral $ EVar . (<> "@f") <$> vars

          replaceFun fvar = everywhereOnErl go
            where
              go (EVar f) | f == fvar = EApp RegularApp (EVar $ f <> "@f") [varTup]
              go (EApp RegularApp lazyFactory [ ])
               | lazyFactory == EAtomLiteral (Atom Nothing $ identToAtomName $ InternalIdent RuntimeLazyFactory)
               , needLazyRef
               = EApp RegularApp lazyFactory [ EVar lazyVarName ]
              go e = e

      funs <- forM vals $ \((_, ident), val) -> do
        erl <- valueToErl' Nothing val
        let erl' = foldr replaceFun erl vars
        let fun = EFunFull (Just "Reccase") [(EFunBinder [varTup] Nothing, erl')]
        pure $ EVarBind (identToVar ident <> "@f") fun
      let rebinds = map (\var -> EVarBind var (EApp RegularApp (EVar $ var <> "@f") [varTup])) vars
          -- TODO this is not unique in the case of multiple recursive binding groups in same scope
          -- And deduplicating would also be incorrect for overridden idents
          ctxRef = [ EVarBind lazyVarName $ qualFunCall "erlang" "make_ref" [] | needLazyRef ]
      pure $ ctxRef ++ funs ++ rebinds

    qualifiedToVar (Qualified _ ident) = identToVar ident

    qualifiedToTypeclassCtor :: Qualified Ident -> Atom
    qualifiedToTypeclassCtor (Qualified (P.ByModuleName mn') ident)
      -- this case could be always qualified, but is not to assist optimisation
      | mn == mn' =
        Atom Nothing (runIdent' ident)
      | otherwise =
        Atom (Just $ atomModuleName mn' PureScriptModule) (runIdent' ident)
    qualifiedToTypeclassCtor (Qualified (P.BySourcePos  _) ident) = Atom Nothing (runIdent' ident)

    valueToErl :: Expr Ann -> m Erl
    valueToErl = valueToErl' Nothing

    valueToErl' :: Maybe Ident -> Expr Ann -> m Erl
    valueToErl' _ (Literal (pos, _, _, _) l) =
      rethrowWithPosition pos $ literalToValueErl l
    valueToErl' _ (Var _ (Qualified (P.ByModuleName C.Prim) (Ident undef)))
      | undef == C.undefined =
        return $ EAtomLiteral $ Atom Nothing C.undefined
    valueToErl' _ (Var (_, _, _, Just (IsConstructor _ [])) (Qualified _ ident)) =
      return $ constructorLiteral (runIdent' ident) []
    valueToErl' _ (Var _ ident) | isTopLevelBinding ident = pure $
      case M.lookup ident arities of
        Just (Arity (0, 1)) -> EFunRef (qualifiedToErl mn ident) 1
        _
          | Just (EffFnXArity arity) <- effFnArity ident,
            arity > 0 ->
            EFunRef (qualifiedToErl mn ident) arity
        _
          | Just (FnXArity arity) <- fnArity ident,
            arity > 0 ->
            EFunRef (qualifiedToErl mn ident) arity
        _ -> EApp RegularApp (EAtomLiteral $ qualifiedToErl mn ident) []
    valueToErl' _ (Var _ ident) = return $ EVar $ qualifiedToVar ident
    valueToErl' ident (Abs _ arg val) = do
      ret <- valueToErl val

      -- TODO this is mangled in corefn json
      let fixIdent (Ident "$__unused") = UnusedIdent
          fixIdent x = x
          arg' = case fixIdent arg of
            UnusedIdent -> "_"
            _ -> identToVar arg
      return $ EFun1 (fmap identToVar ident) arg' ret
    valueToErl' _ (Accessor _ prop val) = do
      eval <- valueToErl val
      return $ EApp RegularApp (EAtomLiteral $ Atom (Just "maps") "get") [EAtomLiteral $ AtomPS Nothing prop, eval]
    valueToErl' _ (ObjectUpdate _ o ps) = do
      obj <- valueToErl o
      sts <- mapM (sndM valueToErl) ps
      return $ EMapUpdate obj (map (first (AtomPS Nothing)) sts)
    valueToErl' _ e@(App (_, _, _, meta) _ _) = do
      let (f, args) = unApp e []
          eMeta = case meta of
                          Just IsSyntheticApp -> SyntheticApp
                          _ -> RegularApp
      args' <- mapM valueToErl args
      case f of
        Var (_, _, _, Just IsNewtype) _ ->
          return $ head args'
        Var (_, _, _, Just (IsConstructor _ fields)) (Qualified _ ident)
          | length args == length fields ->
            return $ constructorLiteral (runIdent' ident) args'
        Var (_, _, _, Just IsTypeClassConstructor) name -> do
          let res = curriedApp args' $ EApp eMeta (EAtomLiteral $ qualifiedToTypeclassCtor name) []

              replaceQualifiedSelfCalls = \case
                (EAtomLiteral (Atom (Just emod) x)) | emod == atomModuleName mn PureScriptModule -> EAtomLiteral (Atom Nothing x)
                other -> other
          pure $ everywhereOnErl replaceQualifiedSelfCalls res

        -- fully saturated call to foreign import
        Var _ (Qualified _ ident)
          | isFullySaturatedForeignCall mn actualForeignArities f args ->
            return $ EApp eMeta (EAtomLiteral $ qualifiedToErl' mn ForeignModule ident) args'
        -- Fully saturated (consuming tc dicts and value args in 1 call) (including "over-saturated")
        Var _ qi@(Qualified _ _)
          | Just (Arity (n, m)) <- M.lookup qi arities,
            let arity = n + m,
            length args >= arity ->
            return $ curriedApp (drop arity args') $ EApp eMeta (EAtomLiteral (qualifiedToErl mn qi)) (take arity args')
        -- partially saturated application (all tc dicts to be applied in 1 call and maybe more to be applied to the curried result)
        Var _ qi@(Qualified _ _)
          | Just (Arity (n, _)) <- M.lookup qi arities,
            length args >= n,
            n > 0 ->
            return $ curriedApp (drop n args') $ EApp eMeta (EAtomLiteral (qualifiedToErl mn qi)) (take n args')
        _ -> curriedApp args' <$> valueToErl f
      where
        unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
        unApp (App _ val arg) args = unApp val (arg : args)
        unApp other args = (other, args)
    valueToErl' _ (Case _ values binders) = do
      vals <- mapM valueToErl values
      (exprs, binders', newvals) <- bindersToErl vals binders
      -- let ret = EApp (EFunFull (Just "Case") binders') (vals++newvals)
      let funBinderToBinder = \case
            (EFunBinder [e] Nothing, ee) -> (EBinder e, ee)
            (EFunBinder [e] (Just g), ee) -> (EGuardedBinder e g, ee)
            (EFunBinder es Nothing, ee) -> (EBinder (ETupleLiteral es), ee)
            (EFunBinder es (Just g), ee) -> (EGuardedBinder (ETupleLiteral es) g, ee)
      let ret = case (binders', vals, newvals) of
            (binders'', [val'], []) -> ECaseOf val' (map funBinderToBinder binders'')
            (binders'', _, []) -> ECaseOf (ETupleLiteral vals) (map funBinderToBinder binders'')
            _ -> EApp RegularApp (EFunFull (Just "Case") binders') (vals ++ newvals)
      pure $ case exprs of
        [] -> ret
        _ -> EBlock (exprs ++ [ret])
    valueToErl' _ (Let _ ds val) = do
      ds' <- concat <$> mapM bindToErl ds
      ret <- valueToErl val
      -- TODO:  variables rather than creating temporary scope just for this
      -- TODO: This scope doesn't really work probably if we actually want to shadow parent scope (avoiding siblings is fine)
      return $ iife (ds' ++ [ret])
    valueToErl' _ (Constructor (_, _, _, Just IsNewtype) _ _ _) = error "newtype ctor"
    valueToErl' _ (Constructor _ _ (ProperName ctor) fields) =
      let createFn =
            let body = constructorLiteral ctor ((EVar . identToVar) `map` fields)
             in foldr (EFun1 Nothing . identToVar) body fields
       in pure createFn

    iife exprs = EApp RegularApp (EFun0 Nothing (EBlock exprs)) []

    constructorLiteral name args = ETupleLiteral (EAtomLiteral (Atom Nothing (toAtomName name)) : args)



    literalToValueErl :: Literal (Expr Ann) -> m Erl
    literalToValueErl = fmap fst . literalToValueErl' EMapLiteral (\x -> (,[]) <$> valueToErl x)

    literalToValueErl' :: ([(Atom, Erl)] -> Erl) -> (a -> m (Erl, [b])) -> Literal a -> m (Erl, [b])
    literalToValueErl' _ _ (NumericLiteral n) = pure (ENumericLiteral n, [])
    literalToValueErl' _ _ (StringLiteral s) = pure (EStringLiteral s, [])
    literalToValueErl' _ _ (CharLiteral c) = pure (ECharLiteral c , [])
    literalToValueErl' _ _ (BooleanLiteral b) = pure (boolToAtom b, [])
    literalToValueErl' _ f (ArrayLiteral xs) = do
      args <- mapM f xs
      let array = EListLiteral $ fst <$> args
          binds = snd <$> args
      pure (EApp RegularApp (EAtomLiteral $ Atom (Just "array") "from_list") [array], concat binds)
    literalToValueErl' mapLiteral f (ObjectLiteral ps) = do
      pairs <- mapM (sndM f) ps
      pure (mapLiteral $ map (\(label, (e, _)) -> (AtomPS Nothing label, e)) pairs, concatMap (snd . snd) pairs)

    boolToAtom :: Bool -> Erl
    boolToAtom True = EAtomLiteral $ Atom Nothing "true"
    boolToAtom False = EAtomLiteral $ Atom Nothing "false"

    bindersToErl :: [Erl] -> [CaseAlternative Ann] -> m ([Erl], [(EFunBinder, Erl)], [Erl])
    bindersToErl vals cases = do
      let binderLengths = map (length . caseAlternativeBinders) cases
          maxBinders = maximum binderLengths
      if length (nub binderLengths) > 1
        then traceM $ "Found inconsistent binder lengths: " <> show binderLengths
        else pure ()
      res <- mapM (caseToErl maxBinders) cases
      let arrayVars = map fst $ concatMap (\(_, _, x) -> x) res
          convBinder (count, binds) (_, binders, arrayMatches) =
            (count + length arrayMatches, binds ++ map go binders)
            where
              go (EFunBinder bs z, e) = (EFunBinder (bs ++ padBinds count arrayMatches) z, e)
          padBinds n binds = replicate n (EVar "_") ++ map snd binds ++ replicate (length arrayVars - n - length binds) (EVar "_")
          binders' = snd $ foldl convBinder (0, []) res

      pure (concatMap (\(x, _, _) -> x) res, binders', arrayVars)
      where
        caseToErl :: Int -> CaseAlternative Ann -> m ([Erl], [(EFunBinder, Erl)], [(Erl, Erl)])
        caseToErl numBinders (CaseAlternative binders alt) = do
          let binders' = binders ++ replicate (numBinders - length binders) (NullBinder (nullSourceSpan, [], Nothing, Nothing))
              vars = nub $ concatMap binderVars binders'

          newVars <- map Ident <$> replicateM (length vars) freshNameErl

          -- TODO we replace because case expressions do not introduce a scope for the binders, but to preserve identifier
          -- names we could do so only for those identifiers which are not already fresh here
          -- but we currently don't have a parent scope available
          let (_, replaceExpVars, replaceBinderVars) = everywhereOnValues id (replaceEVars (zip vars newVars)) (replaceBVars (zip vars newVars))

          let binders'' = map replaceBinderVars binders'
              alt' = case replaceExpVars (Case (nullSourceSpan, [], Nothing, Nothing) [] [CaseAlternative [] alt]) of
                        Case _ [] [CaseAlternative [] alt'] -> alt'
                        _ -> internalError "Replacing variables should give the same form back"


          (bs, binderContext) :: ([Erl], [((EFunBinder, [Erl]) -> Erl, (T.Text, Erl))]) <- second concat . unzip <$> mapM binderToErl' binders''

          -- binderContext is bindings required to make pattern matching work in binders, ie converting arrays to lists
          -- let (binderBinds, binderVars) = map ($ EFunBinder bs Nothing) *** map (first EVar) $ unzip binderContext

          let contextStep (binds, bindVars) (mkBind, otherContext) =
                (binds <> [ mkBind (EFunBinder (bs <> map snd bindVars) Nothing, vals ++ map fst bindVars) ], bindVars <> [ first EVar otherContext ])
              (binderBinds, binderVars) = foldl' contextStep ([], []) binderContext


          (es, res) <- case alt' of
            Right e -> do
              e' <- valueToErl e
              pure ([], [(EFunBinder bs Nothing, e')])
            Left guards -> first concat . unzip <$> mapM (guardToErl bs) guards

          pure (es ++ binderBinds, res, binderVars)

        guardToErl :: [Erl] -> (Expr Ann, Expr Ann) -> m ([Erl], (EFunBinder, Erl))
        guardToErl bs (ge, e) = do
          var <- freshNameErl
          ge' <- valueToErl ge
          let binder = EFunBinder bs Nothing
              fun =
                EFunFull
                  (Just "Guard")
                  ( (binder, ge') :
                      [(EFunBinder (replicate (length bs) (EVar "_")) Nothing, boolToAtom False) | not (irrefutable binder)]
                  )
              cas = EApp RegularApp fun vals
          e' <- valueToErl e
          pure ([EVarBind var cas], (EFunBinder bs (Just $ Guard $ EVar var), e'))

    binderVars :: Binder Ann -> [Ident]
    binderVars (VarBinder _ ident) = [ident]
    binderVars (NamedBinder _ ident binder) = ident : binderVars binder
    binderVars (LiteralBinder _ (ArrayLiteral es)) = concatMap binderVars es
    binderVars (LiteralBinder _ (ObjectLiteral fields)) = concatMap (binderVars . snd) fields
    binderVars (LiteralBinder _ _) = []
    binderVars (ConstructorBinder _ _ _ binders) = concatMap binderVars binders
    binderVars (NullBinder _) = []

    replaceEVars :: [(Ident, Ident)] -> Expr Ann -> Expr Ann
    replaceEVars vars (Var a (Qualified q@(P.BySourcePos  _) x)) | x `notElem` topLevelNames = Var a $ Qualified q $ fromMaybe x $ lookup x vars
    replaceEVars _ z = z

    replaceBVars :: [(Ident, Ident)] -> Binder Ann -> Binder Ann
    replaceBVars vars (VarBinder a x) = VarBinder a $ fromMaybe x $ lookup x vars
    replaceBVars vars (NamedBinder a x b) = NamedBinder a (fromMaybe x $ lookup x vars) b
    replaceBVars _ z = z

    binderToErl' :: Binder Ann -> m (Erl, [((EFunBinder, [Erl]) -> Erl, (T.Text, Erl))])
    binderToErl' (NullBinder _) = pure (EVar "_", [])
    binderToErl' (VarBinder _ ident) = pure (EVar $ identToVar ident, [])
    binderToErl' (LiteralBinder _ (ArrayLiteral es)) = do
      x <- freshNameErl
      args' <- mapM binderToErl' es

      let arrayToList = EAtomLiteral $ Atom (Just "array") "to_list"
          cas (binder, vals) =
            EApp RegularApp
              ( EFunFull
                  Nothing
                  ( (binder, EApp RegularApp arrayToList [EVar x]) :
                      [(EFunBinder (replicate (length vals) (EVar "_")) Nothing, EAtomLiteral $ Atom Nothing "fail") | not (irrefutable binder)]
                  )
              )
              vals
      var <- freshNameErl

      let arr = EListLiteral (map fst args')
      pure (EVar x, (EVarBind var . cas, (var, arr)) : concatMap snd args')
    binderToErl' (LiteralBinder _ lit) = literalToValueErl' EMapPattern binderToErl' lit
    binderToErl' (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) = binderToErl' b
    binderToErl' (ConstructorBinder _ _ (Qualified _ (ProperName ctorName)) binders) = do
        args' <- mapM binderToErl' binders
        pure (constructorLiteral ctorName (map fst args'), concatMap snd args')
    binderToErl' (NamedBinder _ ident binder) = do
      (e, xs) <- binderToErl' binder
      pure (EVarBind (identToVar ident) e, xs)

    irrefutable (EFunBinder bindEs Nothing) = all isOk bindEs
      where
        isOk (EVarBind _ e) = isOk e
        isOk (EVar _) = True
        isOk _ = False
    irrefutable _ = False

    ensureFreshVars_ = ensureFreshVars Set.empty M.empty

    ensureFreshVars :: Set Ident -> Map Ident Ident -> Expr Ann -> m (Expr Ann)
    ensureFreshVars scope vars = go
      where
        go :: Expr Ann -> m (Expr Ann)
        go expr = case expr of
          Literal ann lit ->
            Literal ann <$> case lit of
              ArrayLiteral exs -> ArrayLiteral <$> traverse go exs
              ObjectLiteral fields -> ObjectLiteral <$> traverse (traverse go) fields
              _ -> pure lit
          Constructor {} -> pure expr
          Accessor ann ps ex -> Accessor ann ps <$> go ex
          ObjectUpdate ann e updates -> ObjectUpdate ann <$> go e <*> traverse (traverse go) updates
          Abs ann ident e -> do
            (ident', scope', vars') <- bindIdent scope vars ident
            Abs ann ident' <$> ensureFreshVars scope' vars' e
          App ann e1 e2 -> App ann <$> go e1 <*> go e2
          (Var a (Qualified q@(P.BySourcePos  _) x)) | x `notElem` topLevelNames -> pure $ Var a $ Qualified q $ fromMaybe x $ M.lookup x vars
          otherVar@Var {} -> pure otherVar
          Case ann es cases -> Case ann <$> traverse go es <*> traverse goCase cases
          Let ann binds e -> do
            (scope', vars', binds') <- foldM goBind (scope, vars, []) binds
            Let ann (reverse binds') <$> ensureFreshVars scope' vars' e

        bindIdent scope vars = \case
          ident@UnusedIdent -> pure (ident, scope, vars)
          Ident "$__unused" -> pure (UnusedIdent, scope, vars)
          ident@P.GenIdent {} -> pure (ident, scope, vars)
          ident@P.InternalIdent {} -> pure (ident, scope, vars)
          ident@(Ident rawIdent) ->
            if lowerIdent ident `Set.member` scope
              then do
                newIdent <- Ident <$> freshNameErl' rawIdent
                pure (newIdent, Set.insert (lowerIdent newIdent) scope, M.insert ident newIdent vars)
              else pure (ident, Set.insert (lowerIdent ident) scope, vars)
          where
            lowerIdent (Ident x) = Ident $ T.toLower x
            lowerIdent other = other

        goBind :: (Set Ident, Map Ident Ident, [Bind Ann]) -> Bind Ann -> m (Set Ident, Map Ident Ident, [Bind Ann])
        goBind (scope, vars, acc) = \case
          NonRec ann ident e -> do
            (ident', scope', vars') <- bindIdent scope vars ident
            e' <- ensureFreshVars scope' vars' e
            pure (scope', vars', NonRec ann ident' e' : acc)
          Rec binds -> do
            let idents = snd . fst <$> binds
            (idents', scope', vars') <-
              foldM
                ( \(accIdents, accScope, accVars) ident -> do
                    (ident', scope', vars') <- bindIdent accScope accVars ident
                    pure (ident' : accIdents, scope', vars')
                )
                ([], scope, vars)
                idents
            let f (newIdent, ((ann, _oldIdent), e)) = ((ann, newIdent),) <$> ensureFreshVars scope' vars' e
            binds' <- traverse f $ zip (reverse idents') binds
            pure (scope', vars', Rec binds' : acc)

        goCase (CaseAlternative ann (Right e)) =
          CaseAlternative ann . Right <$> go e
        goCase (CaseAlternative ann (Left ges)) =
          CaseAlternative ann . Left
            <$> traverse (traverse go) ges
