-- |
-- This module provides basic inlining capabilities
--
module Language.PureScript.Erl.CodeGen.Optimizer.Inliner
  ( inlineCommonValues
  , inlineCommonOperators
  , evaluateIifes
  , etaConvert
  , singleBegin
  , beginBinds
  )
  where


import Prelude.Compat

import Data.Text (Text)
import qualified Data.Text as T
import Data.String (IsString)
import Language.PureScript.PSString (PSString, mkString)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

import Language.PureScript.Erl.CodeGen.AST
import Language.PureScript.Erl.CodeGen.Optimizer.Common
import qualified Language.PureScript.Constants.Prelude as C
import qualified Language.PureScript.Erl.CodeGen.Constants as EC
import Control.Monad.Supply.Class (MonadSupply)
import Language.PureScript.Erl.CodeGen.Common (atomPS)

import qualified Data.Map as Map

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
  convert (EBlock [ EVarBind x e, e2 ]) | happy x e2
    = replaceIdents [(x, e)] e2
  convert e = e

  happy x (EVar x') | x == x' = True
  happy x (EApp e [EVar y]) | x /= y = happy x e
  happy _ _ = False

beginBinds :: Erl -> Erl
beginBinds = everywhereOnErl convert
  where
  convert :: Erl -> Erl
  convert (EApp (EBlock es) args)
    | e:evars <- reverse es
    , all (okBind args) evars
    = EBlock (reverse evars ++ [EApp e args])
  convert other = other

  okBind args ebind = all isEVar args &&
    (case ebind of
      (EVarBind x e) ->
        (not $ x `Set.member` argVars) &&
        (all (\y -> not $ occurs y e) argVars)
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
    convert (EApp (EFunN _ xs e) args)
      | all isEVar args
      , xs `disjoint` mapMaybe unEVar args
      , all (not . flip isRebound e) xs
      , all (not . flip isReboundE e) args
      = renameBoundVars $ replaceIdents (zip xs args) e
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
  convert (EFun1 Nothing x (EApp fun@EFunFull{} [EVar x'])) | x == x', not (occurs x fun) = fun
  convert e = e

inlineCommonValues :: (Erl -> Erl) -> Erl -> Erl
inlineCommonValues expander = everywhereOnErl convert
  where
  convert :: Erl -> Erl
  convert (expander -> EApp fn [dict])
    | isDict semiringInt    dict && isUncurriedFn fnZero fn = ENumericLiteral (Left  0)
    | isDict semiringNumber dict && isUncurriedFn fnZero fn = ENumericLiteral (Right 0.0)
    | isDict semiringInt    dict && isUncurriedFn fnOne  fn = ENumericLiteral (Left  1)
    | isDict semiringNumber dict && isUncurriedFn fnOne  fn = ENumericLiteral (Right 1.0)

    | isDict boundedBoolean dict && isUncurriedFn fnBottom fn = EAtomLiteral $ Atom Nothing "false"
    | isDict boundedBoolean dict && isUncurriedFn fnTop    fn = EAtomLiteral $ Atom Nothing "true"
  convert other = other

  fnZero = (EC.dataSemiring, C.zero)
  fnOne = (EC.dataSemiring, C.one)
  fnBottom = (C.dataBounded, C.bottom)
  fnTop = (C.dataBounded, C.top)

data Binary = Binary (Text, PSString) (Text, PSString) BinaryOperator
data Unary = Unary (Text, PSString) (Text, PSString) UnaryOperator

inlineCommonOperators :: Text -> EC.EffectDictionaries -> (Erl -> Erl) -> Erl -> Erl
inlineCommonOperators effectModule EC.EffectDictionaries{..} expander =
  everywhereOnErlTopDown $ applyAll
  [
    binaryOps expander
  , unaryOps expander

  , inlineNonClassFunction (EC.dataFunction, C.apply) $ \f x -> EApp f [x]
  , inlineNonClassFunction (EC.dataFunction, C.applyFlipped) $ \x f -> EApp f [x]

  , inlineErlAtom

  , unaryFn (effectModule, edFunctor) functorVoid id
  , onNFn
  ]

  where

  unaryFn ::  (Text, PSString) -> (Text, PSString) -> (Erl -> Erl) -> Erl -> Erl
  unaryFn dicts fns f = convert
    where
    convert :: Erl -> Erl
    convert (EApp (EApp (EApp fn []) [EApp dict' []]) [x]) | isFnName dicts dict' && isFnName fns fn = f x
    convert (EApp (EApp fn [EApp dict' []]) [x]) | isFnName dicts dict' && isFnName fns fn = f x
    convert (EApp fn [EApp dict' [], x]) | isFnName dicts dict' && isFnName fns fn = f x
    convert other = other

  inlineNonClassFunction :: (Text, Text) -> (Erl -> Erl -> Erl) -> Erl -> Erl
  inlineNonClassFunction modFn f = convert
    where
    convert :: Erl -> Erl
    convert (EApp (EApp op' [x]) [y]) | isModFn modFn op' = f x y
    convert (EApp op' [x, y]) | isUncurriedFn' modFn op' = f x y
    convert other = other

  inlineErlAtom :: Erl -> Erl
  inlineErlAtom = convert
    where
    convert :: Erl -> Erl
    convert (EApp op' [EStringLiteral s])
            | isModFn (EC.erlAtom, EC.atom) op'
              || isUncurriedFn' (EC.erlAtom, EC.atom) op' =
      EAtomLiteral (AtomPS Nothing s)
    convert other = other

  isModFn :: (Text, Text) -> Erl -> Bool
  isModFn = isFn

binaryOps :: (Erl -> Erl) -> Erl -> Erl
binaryOps expander = \case
  eapp@(EApp fn [dict, opArg1, opArg2])
    | Just op <- getOp fn dict
    -> EBinary op opArg1 opArg2
    | otherwise
    -> eapp

  EApp (EApp (expander -> EApp fn [dict]) [opArg1]) [opArg2]
    | Just op <- getOp fn dict
    -> EBinary op opArg1 opArg2

  other -> other

  where
  getOp (EAtomLiteral (Atom (Just moduleName) fnName)) (EApp (EAtomLiteral (Atom (Just dictModuleName) dictName)) []) =
    Map.lookup ((dictModuleName, dictName), (moduleName, fnName)) binaryOperators
  getOp _ _ = Nothing


unaryOps :: (Erl -> Erl) -> Erl -> Erl
unaryOps expander = \case
  eapp@(EApp fn [dict, opArg])
    | Just op <- getOp fn dict
    -> EUnary op opArg
    | otherwise -> eapp

  EApp (expander -> EApp fn [dict]) [opArg]
    | Just op <- getOp fn dict
    -> EUnary op opArg

  other -> other

  where
  getOp (EAtomLiteral (Atom (Just moduleName) fnName)) (EApp (EAtomLiteral (Atom (Just dictModuleName) dictName)) []) =
    Map.lookup ((dictModuleName, dictName), (moduleName, fnName)) unaryOperators
  getOp _ _ = Nothing

onNFn :: Erl -> Erl
onNFn = convert where
  convert (EApp mkFnN [EFun1 Nothing _ e])
    | (EAtomLiteral (Atom (Just mkMod) mkFun)) <- mkFnN
    , Just (MkFnN 0 res) <- Map.lookup (mkMod, mkFun) fnNs
    = res [] e
  convert (EApp mkFnN [ fn ])
    | (EAtomLiteral (Atom (Just mkMod) mkFun)) <- mkFnN
    , Just (MkFnN n res) <- Map.lookup (mkMod, mkFun) fnNs
    , Just (args, e) <- collectArgs n n [] fn
    = res args e
  convert (EApp runFnN (fn : args ))
    | (EAtomLiteral (Atom (Just runMod) runFun)) <- runFnN
    , Just (RunFnN n res) <- Map.lookup (runMod, runFun) fnNs
    , length args == n
    = res (normaliseRef n fn) args
  convert other = other

  collectArgs :: Int -> Int -> [Text] -> Erl -> Maybe ([Text], Erl)
  collectArgs n 1 acc (EFun1 Nothing arg e) | length acc == n - 1 = Just (reverse (arg : acc), e)
  collectArgs n m acc (EFun1 Nothing arg e) = collectArgs n (m - 1) (arg : acc) e
  collectArgs _ _ _   _ = Nothing

  normaliseRef n (EFunRef _ arity) | arity /= n = error "Should never see wrong arity here"
  normaliseRef _ (EFunRef fn _) = EAtomLiteral fn
  normaliseRef _ other = other

data FnNRes = MkFnN Int ([Text] -> Erl -> Erl) | RunFnN Int (Erl -> [Erl] -> Erl)

fnNs :: Map.Map (Text, Text) FnNRes
fnNs = Map.fromList $
  [ fn | i <- [0..10], fn <-
    [ ( (EC.dataFunctionUncurried, name C.mkFn i), MkFnN i $ \args e -> EFunN Nothing args e )
    , ( (EC.effectUncurried, name C.mkEffectFn i), MkFnN i $ \args e -> EFunN Nothing args (EApp e []) )
    ]
  ] ++
  [ fn | i <- [1..10], fn <-
    [ ( (EC.dataFunctionUncurried, name C.runFn i), RunFnN i EApp )
    , ( (EC.effectUncurried, name C.runEffectFn i), RunFnN i $ \fn acc -> EFun0 Nothing (EApp fn acc) )
    ]
  ]
  where
  name prefix n = atomPS $ mkString $ prefix <> T.pack (show n)

binaryOperators :: Map.Map ((Text, Text), (Text, Text)) BinaryOperator
binaryOperators = Map.fromList $ (\(Binary (dmod, dfn) (omod, ofn) result) -> (((dmod, atomPS dfn),(omod, atomPS ofn)), result)) <$>
  (
    [ Binary euclideanRingNumber opDiv FDivide
    , Binary euclideanRingInt opDiv IDivide

    , Binary heytingAlgebraBoolean opConj AndAlso
    , Binary heytingAlgebraBoolean opDisj OrElse
    ]
    ++
      concatMap
      (\(semi, ring) ->
        [ Binary semi opAdd Add
        , Binary semi opMul Multiply
        , Binary ring opSub Subtract
        ]
      )
      [ (semiringNumber, ringNumber), (semiringInt, ringInt ) ]
    ++
      concatMap
      (\eq ->
        [ Binary eq opEq IdenticalTo
        , Binary eq opNotEq NotIdenticalTo
        ]
      )
      [ eqNumber, eqInt, eqString, eqChar, eqBoolean ]
    ++
      concatMap
      (\ord ->
        [ Binary ord opLessThan LessThan
        , Binary ord opLessThanOrEq LessThanOrEqualTo
        , Binary ord opGreaterThan GreaterThan
        , Binary ord opGreaterThanOrEq GreaterThanOrEqualTo
        ]
      )
      [ ordBoolean, ordChar, ordInt, ordNumber, ordString ]
  )

unaryOperators :: Map.Map ((Text, Text), (Text, Text)) UnaryOperator
unaryOperators = Map.fromList $ (\(Unary (dmod, dfn) (omod, ofn) result) -> (((dmod, atomPS dfn),(omod, atomPS ofn)), result)) <$>
    [ Unary ringNumber opNegate Negate
    , Unary ringInt opNegate Negate
    , Unary heytingAlgebraBoolean opNot Not
    ]

semiringNumber :: forall a b. (IsString a, IsString b) => (a, b)
semiringNumber = (EC.dataSemiring, C.semiringNumber)

semiringInt :: forall a b. (IsString a, IsString b) => (a, b)
semiringInt = (EC.dataSemiring, C.semiringInt)

ringNumber :: forall a b. (IsString a, IsString b) => (a, b)
ringNumber = (EC.dataRing, C.ringNumber)

ringInt :: forall a b. (IsString a, IsString b) => (a, b)
ringInt = (EC.dataRing, C.ringInt)

euclideanRingNumber :: forall a b. (IsString a, IsString b) => (a, b)
euclideanRingNumber = (EC.dataEuclideanRing, C.euclideanRingNumber)

euclideanRingInt :: forall a b. (IsString a, IsString b) => (a, b)
euclideanRingInt = (EC.dataEuclideanRing, EC.euclideanRingInt)

eqNumber :: forall a b. (IsString a, IsString b) => (a, b)
eqNumber = (EC.dataEq, C.eqNumber)

eqInt :: forall a b. (IsString a, IsString b) => (a, b)
eqInt = (EC.dataEq, C.eqInt)

eqString :: forall a b. (IsString a, IsString b) => (a, b)
eqString = (EC.dataEq, C.eqString)

eqChar :: forall a b. (IsString a, IsString b) => (a, b)
eqChar = (EC.dataEq, C.eqChar)

eqBoolean :: forall a b. (IsString a, IsString b) => (a, b)
eqBoolean = (EC.dataEq, C.eqBoolean)

ordBoolean :: forall a b. (IsString a, IsString b) => (a, b)
ordBoolean = (EC.dataOrd, C.ordBoolean)

ordNumber :: forall a b. (IsString a, IsString b) => (a, b)
ordNumber = (EC.dataOrd, C.ordNumber)

ordInt :: forall a b. (IsString a, IsString b) => (a, b)
ordInt = (EC.dataOrd, C.ordInt)

ordString :: forall a b. (IsString a, IsString b) => (a, b)
ordString = (EC.dataOrd, C.ordString)

ordChar :: forall a b. (IsString a, IsString b) => (a, b)
ordChar = (EC.dataOrd, C.ordChar)

-- semigroupString :: forall a b. (IsString a, IsString b) => (a, b)
-- semigroupString = (EC.dataSemigroup, C.semigroupString)

boundedBoolean :: forall a b. (IsString a, IsString b) => (a, b)
boundedBoolean = (EC.dataBounded, C.boundedBoolean)

heytingAlgebraBoolean :: forall a b. (IsString a, IsString b) => (a, b)
heytingAlgebraBoolean = (EC.dataHeytingAlgebra, C.heytingAlgebraBoolean)

-- semigroupoidFn :: forall a b. (IsString a, IsString b) => (a, b)
-- semigroupoidFn = (EC.controlSemigroupoid, C.semigroupoidFn)

opAdd :: forall a b. (IsString a, IsString b) => (a, b)
opAdd = (EC.dataSemiring, C.add)

opMul :: forall a b. (IsString a, IsString b) => (a, b)
opMul = (EC.dataSemiring, C.mul)

opEq :: forall a b. (IsString a, IsString b) => (a, b)
opEq = (EC.dataEq, C.eq)

opNotEq :: forall a b. (IsString a, IsString b) => (a, b)
opNotEq = (EC.dataEq, C.notEq)

opLessThan :: forall a b. (IsString a, IsString b) => (a, b)
opLessThan = (EC.dataOrd, C.lessThan)

opLessThanOrEq :: forall a b. (IsString a, IsString b) => (a, b)
opLessThanOrEq = (EC.dataOrd, C.lessThanOrEq)

opGreaterThan :: forall a b. (IsString a, IsString b) => (a, b)
opGreaterThan = (EC.dataOrd, C.greaterThan)

opGreaterThanOrEq :: forall a b. (IsString a, IsString b) => (a, b)
opGreaterThanOrEq = (EC.dataOrd, C.greaterThanOrEq)

-- opAppend :: forall a b. (IsString a, IsString b) => (a, b)
-- opAppend = (EC.dataSemigroup, C.append)

opSub :: forall a b. (IsString a, IsString b) => (a, b)
opSub = (EC.dataRing, C.sub)

opNegate :: forall a b. (IsString a, IsString b) => (a, b)
opNegate = (EC.dataRing, C.negate)

opDiv :: forall a b. (IsString a, IsString b) => (a, b)
opDiv = (EC.dataEuclideanRing, C.div)

opConj :: forall a b. (IsString a, IsString b) => (a, b)
opConj = (EC.dataHeytingAlgebra, C.conj)

opDisj :: forall a b. (IsString a, IsString b) => (a, b)
opDisj = (EC.dataHeytingAlgebra, C.disj)

opNot :: forall a b. (IsString a, IsString b) => (a, b)
opNot = (EC.dataHeytingAlgebra, C.not)

functorVoid :: forall a b. (IsString a, IsString b) => (a, b)
functorVoid = (EC.dataFunctor, EC.void)