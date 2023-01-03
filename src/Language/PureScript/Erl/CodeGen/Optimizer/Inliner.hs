-- |
-- This module provides basic inlining capabilities
module Language.PureScript.Erl.CodeGen.Optimizer.Inliner
  ( inlineCommonValues,
    inlineCommonOperators,
    inlineCommonFnsM,
    evaluateIifes,
    etaConvert,
    singleBegin,
    beginBinds,
    collectLists,
    replaceAppliedFunRefs,
  )
where

import Control.Monad.Supply.Class (MonadSupply (fresh))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.PureScript.Constants.Prim as C
import qualified Language.PureScript.Constants.Libs as C
import Language.PureScript.Erl.CodeGen.AST
import Language.PureScript.Erl.CodeGen.Common (atomPS, runAtom)
import qualified Language.PureScript.Erl.CodeGen.Constants as EC
import Language.PureScript.Erl.CodeGen.Optimizer.Common
import Language.PureScript.PSString (PSString, mkString)
import Prelude.Compat

isEVar :: Erl -> Bool
isEVar (EVar _) = True
isEVar _ = False

unEVar :: Erl -> Maybe Text
unEVar (EVar x) = Just x
unEVar _ = Nothing

-- inline as we generate this for FFI calls
-- begin X = E, X(A)(B)... end
singleBegin :: Erl -> Erl
singleBegin = everywhereOnErl convert
  where
    convert :: Erl -> Erl
    convert (EBlock [EVarBind x e, e2])
      | happy x e2 =
        replaceIdents [(x, e)] e2
    convert e = e

    happy x (EVar x') | x == x' = True
    happy x (EApp _ e [EVar y]) | x /= y = happy x e
    happy _ _ = False

beginBinds :: Erl -> Erl
beginBinds = everywhereOnErl convert
  where
    convert :: Erl -> Erl
    convert (EApp meta (EBlock es) args)
      | e : evars <- reverse es,
        all (okBind args) evars =
        EBlock (reverse evars ++ [EApp meta e args])
    convert other = other

    okBind args ebind =
      all isEVar args
        && ( case ebind of
               (EVarBind x e) ->
                 not (x `Set.member` argVars)
                   && all (\y -> not $ occurs y e) argVars
               _ -> False
           )
      where
        argVars = Set.fromList $ mapMaybe unEVar args

-- (begin X1 = E1, ... Xn = En, F end)(X)
-- to
-- begin X1 = E1, ... Xn = En, F(X) end
-- X /= Xi, X \notin FV(Ei)

etaConvert :: MonadSupply m => Erl -> m Erl
etaConvert = everywhereOnErlTopDownM convert
  where
    convert :: MonadSupply m => Erl -> m Erl
    -- TODO ported from JS, but this seems to be beta-reduction and the iife below is eta...?
    convert (EApp _ (EFunN _ xs e) args)
      | all isEVar args,
        xs `disjoint` mapMaybe unEVar args,
        not (any (`isRebound` e) xs),
        not (any (`isReboundE` e) args) =
        renameBoundVars $ replaceIdents (zip xs args) e
    convert e = pure e

    disjoint l1 l2 =
      Set.null $ s1 `Set.intersection` s2
      where
        s1 = Set.fromList l1
        s2 = Set.fromList l2

    isReboundE (EVar x) e = isRebound x e
    isReboundE _ _ = False

-- TODO: That's not iifes
-- \x. (f x) --eta--> f  (x \notelem FV(f))
-- -- fun (X) -> fun {body} end(X) end  --> fun {body} end
evaluateIifes :: Erl -> Erl
evaluateIifes = everywhereOnErl convert
  where
    convert :: Erl -> Erl
    convert (EFun1 Nothing x (EApp _ fun@EFunFull {} [EVar x'])) | x == x', not (occurs x fun) = fun
    convert e = e

collectLists :: Erl -> Erl
collectLists = everywhereOnErl go
  where
    go (EListCons xs (EListLiteral ys)) = EListLiteral (xs <> ys)
    go (EListCons xs (EListCons ys z)) = EListCons (xs <> ys) z
    go (EBinary ListConcat (EListLiteral xs) (EListLiteral ys)) = EListLiteral (xs <> ys)
    go (EBinary ListConcat (EListLiteral xs) (EListCons ys z)) = EListCons (xs <> ys) z
    go other = other

replaceAppliedFunRefs :: Erl -> Erl
replaceAppliedFunRefs = everywhereOnErl go
  where
    go (EApp meta (EFunRef name arity) args)
      | length args == arity =
        EApp meta (EAtomLiteral name) args
    go other = other

inlineCommonValues :: (Erl -> Erl) -> Erl -> Erl
inlineCommonValues expander = everywhereOnErl convert
  where
    convert :: Erl -> Erl
    convert (expander -> EApp _ fn [dict])
      | isDict semiringInt dict && isUncurriedFn fnZero fn = ENumericLiteral (Left 0)
      | isDict semiringNumber dict && isUncurriedFn fnZero fn = ENumericLiteral (Right 0.0)
      | isDict semiringInt dict && isUncurriedFn fnOne fn = ENumericLiteral (Left 1)
      | isDict semiringNumber dict && isUncurriedFn fnOne fn = ENumericLiteral (Right 1.0)
      | isDict boundedBoolean dict && isUncurriedFn fnBottom fn = EAtomLiteral $ Atom Nothing "false"
      | isDict boundedBoolean dict && isUncurriedFn fnTop fn = EAtomLiteral $ Atom Nothing "true"
    convert fn
      | isFn (EC.dataUnit, EC.unit) fn = EAtomLiteral $ Atom Nothing "unit"
      | isFn (EC.erlDataMap, EC.empty) fn = EMapLiteral []
      | isFn (EC.erlDataListTypes, EC.nil) fn = EListLiteral []
    convert other = other

    fnZero = (EC.dataSemiring, snd $ C.P_zero)
    fnOne = (EC.dataSemiring, snd $ C.P_one)
    fnBottom = (EC.dataBounded, snd $ C.P_bottom)
    fnTop = (EC.dataBounded, snd $ C.P_top)

data Binary
  = Binary (Text, PSString) (Text, PSString) BinaryOperator
  | BinaryFn (Text, PSString) (Text, PSString) (Erl -> Erl -> Erl)

data Unary = Unary (Text, PSString) (Text, PSString) UnaryOperator

inlineCommonFnsM :: forall m. (Monad m, MonadSupply m) => (Erl -> Erl) -> Erl -> m Erl
inlineCommonFnsM _expander =
  everywhereOnErlTopDownM $
    applyAllM
      [ inlineNonClassFunction3 (EC.dataMaybe, EC.maybe) $ \x f -> inlineMaybe x (\z -> EApp RegularApp f [z]),
        inlineNonClassFunction3 (EC.dataMaybe, EC.maybe') $ \fx f -> inlineMaybe (applyUnit fx) (\z -> EApp RegularApp f [z]),
        inlineNonClassFunction (EC.dataMaybe, EC.fromMaybe) $ \x -> inlineMaybe x id,
        inlineNonClassFunction3 (EC.dataEither, EC.either) $ \l r -> inlineEither (\z -> EApp RegularApp l [z]) (\z -> EApp RegularApp r [z]),
        inlineNonClassFunction (EC.dataEither, EC.fromLeft) $ \r -> inlineEither id (const r),
        inlineNonClassFunction (EC.dataEither, EC.fromRight) $ \l -> inlineEither (const l) id
      ]
  where
    applyUnit (EFun1 _ x e)
      | not (occurs x e) =
        e
    applyUnit fx =
      EApp RegularApp fx [EAtomLiteral $ Atom Nothing "unit"]

    inlineEither l r e = do
      n <- fresh
      let var = EVar $ "_E" <> "@" <> T.pack (show n)

      pure $
        ECaseOf
          e
          [ (EBinder (ETupleLiteral [EAtomLiteral (Atom Nothing "left"), var]), l var),
            (EBinder (ETupleLiteral [EAtomLiteral (Atom Nothing "right"), var]), r var)
          ]
    inlineMaybe x f m = do
      n <- fresh
      let var = EVar $ "_J" <> "@" <> T.pack (show n)

      pure $
        ECaseOf
          m
          [ (EBinder (ETupleLiteral [EAtomLiteral (Atom Nothing "nothing")]), x),
            (EBinder (ETupleLiteral [EAtomLiteral (Atom Nothing "just"), var]), f var)
          ]

    inlineNonClassFunction3 :: (Text, Text) -> (Erl -> Erl -> Erl -> m Erl) -> Erl -> m Erl
    inlineNonClassFunction3 modFn f = convert
      where
        convert :: Erl -> m Erl
        convert (EApp _ (EApp _ (EApp _ op' [x]) [y]) [z]) | isFn modFn op' = f x y z
        convert (EApp _ op' [x, y, z]) | isUncurriedFn' modFn op' = f x y z
        convert other = pure other

    inlineNonClassFunction :: (Text, Text) -> (Erl -> Erl -> m Erl) -> Erl -> m Erl
    inlineNonClassFunction modFn f = convert
      where
        convert :: Erl -> m Erl
        convert (EApp _ (EApp _ op' [x]) [y]) | isFn modFn op' = f x y
        convert (EApp _ op' [x, y]) | isUncurriedFn' modFn op' = f x y
        convert other = pure other

inlineCommonOperators :: Text -> EC.EffectDictionaries -> (Erl -> Erl) -> Erl -> Erl
inlineCommonOperators effectModule EC.EffectDictionaries {..} expander =
  everywhereOnErlTopDown $
    applyAll
      [ binaryOps expander,
        unaryOps expander,
        inlineNonClassFunction (EC.erlDataListTypes, EC.cons) $ \x xs -> EListCons [x] xs,
        inlineNonClassUnaryFunction (EC.erlDataListTypes, EC.null) $ \x -> EBinary EqualTo (EListLiteral []) x,
        inlineNonClassUnaryFunction (EC.erlDataList, EC.singleton) $ \x -> EListLiteral [x],
        inlineErlAtom,
        unaryFn (effectModule, edFunctor) functorVoid id,
        inlineNonClassUnaryFunction (EC.unsafeCoerceMod, EC.unsafeCoerce) id,
        inlineNonClassUnaryFunction (EC.dataInt, EC.toNumber) $ \x -> EApp RegularApp erlangFloat [x],
        unaryUndefTCFn (EC.safeCoerceMod, EC.coerce) id,
        unaryUndefTCFn (EC.dataNewtype, EC.unwrap) id,
        unaryUndefTCFn (EC.dataNewtype, EC.wrap) id,
        binaryUndefTC2Fn (EC.dataNewtype, EC.over) $ \_ x -> x,
        binaryUndefTC2Fn (EC.dataNewtype, EC.over2) $ \_ x -> x,
        inlineDiscardUnit,
        onNFn expander,
        onTupleN
      ]
  where
    unaryFn :: (Text, PSString) -> (Text, PSString) -> (Erl -> Erl) -> Erl -> Erl
    unaryFn dicts fns f = convert
      where
        convert :: Erl -> Erl
        convert (EApp _ (EApp _ (EApp _ fn []) [dict]) [x]) | isDict dicts dict && isFnName fns fn = f x
        convert (EApp _ (EApp _ fn [dict]) [x]) | isDict dicts dict && isFnName fns fn = f x
        convert (EApp _ fn [dict, x]) | isFnName dicts dict && isFnName fns fn = f x
        convert other = other

    unaryUndefTCFn :: (Text, PSString) -> (Erl -> Erl) -> Erl -> Erl
    unaryUndefTCFn fns f = convert
      where
        convert :: Erl -> Erl
        convert (EApp _ (expander -> EApp _ fn [undef]) [x]) | isUndef undef && isFnName fns fn = f x
        convert (EApp _ fn [undef, x]) | isUndef undef && isFnName fns fn = f x
        convert other = other

        isUndef (EAtomLiteral atom) | runAtom atom == C.S_undefined = True
        isUndef _ = False

    binaryUndefTC2Fn :: (Text, PSString) -> (Erl -> Erl -> Erl) -> Erl -> Erl
    binaryUndefTC2Fn fns f = convert
      where
        convert :: Erl -> Erl
        convert (EApp _ (EApp _ (expander -> EApp _ fn [undef, undef']) [x]) [y]) | isUndef undef && isUndef undef' && isFnName fns fn = f x y
        convert (EApp _ fn [undef, undef', x, y]) | isUndef undef && isUndef undef' && isFnName fns fn = f x y
        convert other = other

        isUndef (EAtomLiteral atom) | runAtom atom == C.S_undefined = True
        isUndef _ = False

    inlineNonClassFunction :: (Text, Text) -> (Erl -> Erl -> Erl) -> Erl -> Erl
    inlineNonClassFunction modFn f = convert
      where
        convert :: Erl -> Erl
        convert (EApp _ (EApp _ op' [x]) [y]) | isModFn modFn op' = f x y
        convert (EApp _ op' [x, y]) | isUncurriedFn' modFn op' = f x y
        convert other = other

    inlineNonClassUnaryFunction :: (Text, Text) -> (Erl -> Erl) -> Erl -> Erl
    inlineNonClassUnaryFunction modFn f = convert
      where
        convert :: Erl -> Erl
        convert (EApp _ op' [x]) | isModFn modFn op' = f x
        convert (EApp _ op' [x]) | isUncurriedFn' modFn op' = f x
        convert other = other

    inlineErlAtom :: Erl -> Erl
    inlineErlAtom = convert
      where
        convert :: Erl -> Erl
        convert (EApp _ op' [EStringLiteral s])
          | isModFn (EC.erlAtom, EC.atom) op'
              || isUncurriedFn' (EC.erlAtom, EC.atom) op' =
            EAtomLiteral (AtomPS Nothing s)
        convert other = other

    isModFn :: (Text, Text) -> Erl -> Bool
    isModFn = isFn

    inlineDiscardUnit :: Erl -> Erl
    inlineDiscardUnit = go
      where
        go eApp@EApp {} = case eApp of
          (collect 2 . expander -> EApp meta fn [dict1, dict2])
            | isDict (EC.controlBind, EC.discardUnit) dict1 && isFn (EC.controlBind, snd $ C.P_discard) fn ->
              EApp meta controlBindBind [dict2]
          _ -> eApp
        go other = other
        controlBindBind = EAtomLiteral (Atom (Just EC.controlBind) C.S_bind)

binaryOps :: (Erl -> Erl) -> Erl -> Erl
binaryOps expander = \case
  eapp@(EApp _ fn [dict, opArg1, opArg2])
    | Just op <- getOp fn dict ->
      res op opArg1 opArg2
    | otherwise ->
      eapp
  EApp _ (EApp _ (expander -> EApp _ fn [dict]) [opArg1]) [opArg2]
    | Just op <- getOp fn dict ->
      res op opArg1 opArg2
  other -> other
  where
    res :: Either BinaryOperator (Erl -> Erl -> Erl) -> Erl -> Erl -> Erl
    res (Left op) = EBinary op
    res (Right f) = f

    getOp (EAtomLiteral (Atom (Just moduleName) fnName)) (EApp _ (EAtomLiteral (Atom (Just dictModuleName) dictName)) []) =
      Map.lookup ((dictModuleName, dictName), (moduleName, fnName)) binaryOperators
    getOp _ _ = Nothing

unaryOps :: (Erl -> Erl) -> Erl -> Erl
unaryOps expander = \case
  eapp@(EApp _ fn [dict, opArg])
    | Just op <- getOp fn dict ->
      EUnary op opArg
    | otherwise -> eapp
  EApp _ (expander -> EApp _ fn [dict]) [opArg]
    | Just op <- getOp fn dict ->
      EUnary op opArg
  other -> other
  where
    getOp (EAtomLiteral (Atom (Just moduleName) fnName)) (EApp _ (EAtomLiteral (Atom (Just dictModuleName) dictName)) []) =
      Map.lookup ((dictModuleName, dictName), (moduleName, fnName)) unaryOperators
    getOp _ _ = Nothing

onNFn :: (Erl -> Erl) -> Erl -> Erl
onNFn expander = convert
  where
    convert (EApp _ mkFnN [EFun1 Nothing _ e])
      | (EAtomLiteral (Atom (Just mkMod) mkFun)) <- mkFnN,
        Just (MkFnN 0 res) <- Map.lookup (mkMod, mkFun) fnNs =
        res [] e
    convert (EApp _ mkFnN [fn])
      | (EAtomLiteral (Atom (Just mkMod) mkFun)) <- mkFnN,
        Just (MkFnN n res) <- Map.lookup (mkMod, mkFun) fnNs,
        Just (args, e) <- collectArgs n n [] fn =
        res args e
    convert (expander -> EApp _ runFnN (fn : args))
      | (EAtomLiteral (Atom (Just runMod) runFun)) <- runFnN,
        Just (RunFnN n res) <- Map.lookup (runMod, runFun) fnNs,
        length args == n =
        res fn args
    convert other = other

    collectArgs :: Int -> Int -> [Text] -> Erl -> Maybe ([Text], Erl)
    collectArgs n 1 acc (EFun1 Nothing arg e) | length acc == n - 1 = Just (reverse (arg : acc), e)
    collectArgs n m acc (EFun1 Nothing arg e) = collectArgs n (m - 1) (arg : acc) e
    collectArgs _ _ _ _ = Nothing

data FnNRes = MkFnN Int ([Text] -> Erl -> Erl) | RunFnN Int (Erl -> [Erl] -> Erl)

fnNs :: Map.Map (Text, Text) FnNRes
fnNs =
  Map.fromList $
    [ fn | i <- [0 .. 10], fn <-
                             [ ((EC.dataFunctionUncurried, name C.S_mkFn i), MkFnN i $ \args e -> EFunN Nothing args e),
                               ((EC.effectUncurried, name (snd C.P_mkEffectFn) i), MkFnN i $ \args e -> EFunN Nothing args (EApp RegularApp e []))
                             ]
    ]
      ++ [ fn | i <- [1 .. 10], fn <-
                                  [ ((EC.dataFunctionUncurried, name C.S_runFn i), RunFnN i (EApp RegularApp)),
                                    ((EC.effectUncurried, name (snd C.P_runEffectFn) i), RunFnN i $ \fn acc -> EFun0 Nothing (EApp RegularApp fn acc))
                                  ]
         ]
  where
    name prefix n = atomPS $ mkString $ prefix <> T.pack (show n)

onTupleN :: Erl -> Erl
onTupleN = \case
  EApp _ tupleN args
    | (EAtomLiteral (Atom (Just tupleMod) tupleFn)) <- tupleN,
      Just i <- Map.lookup (tupleMod, tupleFn) tupleNs,
      length args == i ->
      ETupleLiteral args
  other -> other

tupleNs :: Map.Map (Text, Text) Int
tupleNs =
  Map.fromList $
    [ fn i | i <- [0 .. 10]
    ]
  where
    fn i = ((EC.erlDataTuple, name EC.tuple i), i)
    name prefix n = atomPS $ mkString $ prefix <> T.pack (show n)

binaryOperators :: Map.Map ((Text, Text), (Text, Text)) (Either BinaryOperator (Erl -> Erl -> Erl))
binaryOperators =
  Map.fromList $
    conv
      <$> ( [ Binary euclideanRingNumber opDiv FDivide,
              Binary euclideanRingInt opDiv IDivide,
              Binary heytingAlgebraBoolean opConj AndAlso,
              Binary heytingAlgebraBoolean opDisj OrElse,
              Binary semigroupList opAppend ListConcat
            ]
              ++ concatMap
                ( \(semi, ring) ->
                    [ Binary semi opAdd Add,
                      Binary semi opMul Multiply,
                      Binary ring opSub Subtract
                    ]
                )
                [(semiringNumber, ringNumber), (semiringInt, ringInt)]
              ++ concatMap
                ( \eq ->
                    [ Binary eq opEq IdenticalTo,
                      Binary eq opNotEq NotIdenticalTo
                    ]
                )
                [eqNumber, eqInt, eqString, eqChar, eqBoolean]
              ++ concatMap
                ( \ord ->
                    [ Binary ord opLessThan LessThan,
                      Binary ord opLessThanOrEq LessThanOrEqualTo,
                      Binary ord opGreaterThan GreaterThan,
                      Binary ord opGreaterThanOrEq GreaterThanOrEqualTo,
                      BinaryFn ord opMin (\x y -> EApp RegularApp erlangMin [x, y]),
                      BinaryFn ord opMax (\x y -> EApp RegularApp erlangMax [x, y])
                    ]
                )
                [ordBoolean, ordChar, ordInt, ordNumber, ordString]
          )
  where
    conv (Binary (dmod, dfn) (omod, ofn) result) = (((dmod, atomPS dfn), (omod, atomPS ofn)), Left result)
    conv (BinaryFn (dmod, dfn) (omod, ofn) result) = (((dmod, atomPS dfn), (omod, atomPS ofn)), Right result)

unaryOperators :: Map.Map ((Text, Text), (Text, Text)) UnaryOperator
unaryOperators =
  Map.fromList $
    (\(Unary (dmod, dfn) (omod, ofn) result) -> (((dmod, atomPS dfn), (omod, atomPS ofn)), result))
      <$> [ Unary ringNumber opNegate Negate,
            Unary ringInt opNegate Negate,
            Unary heytingAlgebraBoolean opNot Not
          ]

semiringNumber :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
semiringNumber = (EC.dataSemiring, snd $ C.P_semiringNumber)

semiringInt :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
semiringInt = (EC.dataSemiring, snd $ C.P_semiringInt)

ringNumber :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
ringNumber = (EC.dataRing, snd $ C.P_ringNumber)

ringInt :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
ringInt = (EC.dataRing, snd $ C.P_ringInt)

euclideanRingNumber :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
euclideanRingNumber = (EC.dataEuclideanRing, snd $ C.P_euclideanRingNumber)

euclideanRingInt :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
euclideanRingInt = (EC.dataEuclideanRing, EC.euclideanRingInt)

eqNumber :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
eqNumber = (EC.dataEq, snd $ C.P_eqNumber)

eqInt :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
eqInt = (EC.dataEq, snd $ C.P_eqInt)

eqString :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
eqString = (EC.dataEq, snd $ C.P_eqString)

eqChar :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
eqChar = (EC.dataEq, snd $ C.P_eqChar)

eqBoolean :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
eqBoolean = (EC.dataEq, snd $ C.P_eqBoolean)

ordBoolean :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
ordBoolean = (EC.dataOrd, snd $ C.P_ordBoolean)

ordNumber :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
ordNumber = (EC.dataOrd, snd $ C.P_ordNumber)

ordInt :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
ordInt = (EC.dataOrd, snd $ C.P_ordInt)

ordString :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
ordString = (EC.dataOrd, snd $ C.P_ordString)

ordChar :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
ordChar = (EC.dataOrd, snd $ C.P_ordChar)

-- semigroupString :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
-- semigroupString = (EC.dataSemigroup, snd $ C.P_semigroupString)

boundedBoolean :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
boundedBoolean = (EC.dataBounded, snd $ C.P_boundedBoolean)

heytingAlgebraBoolean :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
heytingAlgebraBoolean = (EC.dataHeytingAlgebra, snd $ C.P_heytingAlgebraBoolean)

semigroupList :: forall a b. (IsString a, IsString b) => (a, b)
semigroupList = (EC.erlDataListTypes, EC.semigroupList)

-- semigroupoidFn :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
-- semigroupoidFn = (EC.controlSemigroupoid, snd $ C.P_semigroupoidFn)

opAdd :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opAdd = (EC.dataSemiring, snd $ C.P_add)

opMul :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opMul = (EC.dataSemiring, snd $ C.P_mul)

opEq :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opEq = (EC.dataEq, snd $ C.P_eq)

opNotEq :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opNotEq = (EC.dataEq, snd $ C.P_notEq)

opLessThan :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opLessThan = (EC.dataOrd, snd $ C.P_lessThan)

opLessThanOrEq :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opLessThanOrEq = (EC.dataOrd, snd $ C.P_lessThanOrEq)

opGreaterThan :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opGreaterThan = (EC.dataOrd, snd $ C.P_greaterThan)

opGreaterThanOrEq :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opGreaterThanOrEq = (EC.dataOrd, snd $ C.P_greaterThanOrEq)

opMin :: forall a b. (IsString a, IsString b) => (a, b)
opMin = (EC.dataOrd, EC.min)

opMax :: forall a b. (IsString a, IsString b) => (a, b)
opMax = (EC.dataOrd, EC.max)

opAppend :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opAppend = (EC.dataSemigroup, snd $ C.P_append)

opSub :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opSub = (EC.dataRing, snd $ C.P_sub)

opNegate :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opNegate = (EC.dataRing, snd $ C.P_negate)

opDiv :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opDiv = (EC.dataEuclideanRing, snd $ C.P_div)

opConj :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opConj = (EC.dataHeytingAlgebra, snd $ C.P_conj)

opDisj :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opDisj = (EC.dataHeytingAlgebra, snd $ C.P_disj)

opNot :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opNot = (EC.dataHeytingAlgebra, snd $ C.P_not)

functorVoid :: forall a b. (IsString a, IsString b) => (a, b)
functorVoid = (EC.dataFunctor, EC.void)

erlangMin :: Erl
erlangMin = EAtomLiteral (Atom (Just "erlang") "min")

erlangMax :: Erl
erlangMax = EAtomLiteral (Atom (Just "erlang") "max")

erlangFloat :: Erl
erlangFloat = EAtomLiteral (Atom (Just "erlang") "float")
