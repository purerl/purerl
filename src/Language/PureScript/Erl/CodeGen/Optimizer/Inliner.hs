-- |
-- This module provides basic inlining capabilities
--
module Language.PureScript.Erl.CodeGen.Optimizer.Inliner
  ( inlineCommonValues
  , inlineCommonOperators
  , evaluateIifes
  , etaConvert
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

shouldInline :: Erl -> Bool
shouldInline (EVar _) = True
shouldInline _ = False

etaConvert :: MonadSupply m => Erl -> m Erl
etaConvert = everywhereOnErlTopDownM convert
  where
    convert :: MonadSupply m => Erl -> m Erl
    -- TODO ported from JS, but this seems to be beta-reduction and the iife below is eta...?
    convert (EApp (EFunN _ xs e) args)
      | all shouldInline args
      , xs `disjoint` mapMaybe unEVar args
      , all (not . flip isRebound e) xs
      , all (not . flip isReboundE e) args = renameBoundVars $ replaceIdents (zip xs args) e
    convert e = pure e

    unEVar (EVar x) = Just x
    unEVar _ = Nothing

    disjoint l1 l2 =
      Set.null $ s1 `Set.intersection` s2
      where
        s1 = Set.fromList l1
        s2 = Set.fromList l2

    isReboundE (EVar x) e = isRebound x e
    isReboundE _ _ = False

-- TODO: That's not iifes
-- -- fun (X) -> fun {body} end(X) end  --> fun {body} end
evaluateIifes :: Erl -> Erl
evaluateIifes = everywhereOnErl convert
  where
  convert :: Erl -> Erl
  convert (EFun1 Nothing x (EApp fun@EFunFull{} [EVar x'])) | x == x', not (occurs x fun) = fun
  convert e = e

inlineCommonValues :: Erl -> Erl
inlineCommonValues = everywhereOnErl convert
  where
  convert :: Erl -> Erl
  convert (EApp fn [dict])
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

inlineCommonOperators :: Text -> EC.EffectDictionaries -> Erl -> Erl
inlineCommonOperators effectModule EC.EffectDictionaries{..} = 
  everywhereOnErlTopDown $ applyAll $
  [ 
    binaryOps
  , unaryOps
  
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

binaryOps :: Erl -> Erl
binaryOps = convert
  where
  convert :: Erl -> Erl
  convert eapp@(EApp fn [dict', opArg1, opArg2])
    | (EAtomLiteral (Atom (Just moduleName) fnName)) <- fn
    , (EApp (EAtomLiteral (Atom (Just dictModuleName) dictName)) []) <- dict'
    = case Map.lookup ((dictModuleName, dictName), (moduleName, fnName)) binaryOperators of
        Just op -> EBinary op opArg1 opArg2
        Nothing -> eapp
  convert other = other

unaryOps ::  Erl -> Erl
unaryOps = convert
  where
  convert :: Erl -> Erl
  convert eapp@(EApp fn [dict', opArg])
    | (EAtomLiteral (Atom (Just moduleName) fnName)) <- fn
    , (EApp (EAtomLiteral (Atom (Just dictModuleName) dictName)) []) <- dict'
    = case Map.lookup ((dictModuleName, dictName), (moduleName, fnName)) unaryOperators of
        Just op -> EUnary op opArg
        Nothing -> eapp
  convert other = other

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
    [ Binary semiringNumber opAdd Add
    , Binary semiringNumber opMul Multiply
    , Binary ringNumber opSub Subtract
    , Binary semiringInt opAdd Add
    , Binary semiringInt opMul Multiply
    , Binary ringInt opSub Subtract

    , Binary euclideanRingNumber opDiv FDivide

    , Binary eqNumber opEq IdenticalTo
    , Binary eqNumber opNotEq NotIdenticalTo
    , Binary eqInt opEq IdenticalTo
    , Binary eqInt opNotEq NotIdenticalTo
    , Binary eqString opEq IdenticalTo
    , Binary eqString opNotEq NotIdenticalTo
    , Binary eqChar opEq IdenticalTo
    , Binary eqChar opNotEq NotIdenticalTo
    , Binary eqBoolean opEq IdenticalTo
    , Binary eqBoolean opNotEq NotIdenticalTo

    , Binary ordBoolean opLessThan LessThan
    , Binary ordBoolean opLessThanOrEq LessThanOrEqualTo
    , Binary ordBoolean opGreaterThan GreaterThan
    , Binary ordBoolean opGreaterThanOrEq GreaterThanOrEqualTo
    , Binary ordChar opLessThan LessThan
    , Binary ordChar opLessThanOrEq LessThanOrEqualTo
    , Binary ordChar opGreaterThan GreaterThan
    , Binary ordChar opGreaterThanOrEq GreaterThanOrEqualTo
    , Binary ordInt opLessThan LessThan
    , Binary ordInt opLessThanOrEq LessThanOrEqualTo
    , Binary ordInt opGreaterThan GreaterThan
    , Binary ordInt opGreaterThanOrEq GreaterThanOrEqualTo
    , Binary ordNumber opLessThan LessThan
    , Binary ordNumber opLessThanOrEq LessThanOrEqualTo
    , Binary ordNumber opGreaterThan GreaterThan
    , Binary ordNumber opGreaterThanOrEq GreaterThanOrEqualTo
    , Binary ordString opLessThan LessThan
    , Binary ordString opLessThanOrEq LessThanOrEqualTo
    , Binary ordString opGreaterThan GreaterThan
    , Binary ordString opGreaterThanOrEq GreaterThanOrEqualTo

    , Binary heytingAlgebraBoolean opConj And
    , Binary heytingAlgebraBoolean opDisj Or
    ]

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