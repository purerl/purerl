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
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set

import Language.PureScript.Erl.CodeGen.AST
import Language.PureScript.Erl.CodeGen.Optimizer.Common
import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.Erl.CodeGen.Constants as EC
import Control.Monad.Supply.Class (MonadSupply)

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

inlineCommonOperators :: Erl -> Erl
inlineCommonOperators = everywhereOnErlTopDown $ applyAll $
  [ binary semiringNumber opAdd Add
  , binary semiringNumber opMul Multiply
  , binary ringNumber opSub Subtract
  , unary  ringNumber opNegate Negate
  , binary semiringInt opAdd Add
  , binary semiringInt opMul Multiply
  , binary ringInt opSub Subtract
  , unary  ringInt opNegate Negate

  , binary euclideanRingNumber opDiv FDivide

  , binary eqNumber opEq IdenticalTo
  , binary eqNumber opNotEq NotIdenticalTo
  , binary eqInt opEq IdenticalTo
  , binary eqInt opNotEq NotIdenticalTo
  , binary eqString opEq IdenticalTo
  , binary eqString opNotEq NotIdenticalTo
  , binary eqChar opEq IdenticalTo
  , binary eqChar opNotEq NotIdenticalTo
  , binary eqBoolean opEq IdenticalTo
  , binary eqBoolean opNotEq NotIdenticalTo

  , binary ordBoolean opLessThan LessThan
  , binary ordBoolean opLessThanOrEq LessThanOrEqualTo
  , binary ordBoolean opGreaterThan GreaterThan
  , binary ordBoolean opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordChar opLessThan LessThan
  , binary ordChar opLessThanOrEq LessThanOrEqualTo
  , binary ordChar opGreaterThan GreaterThan
  , binary ordChar opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordInt opLessThan LessThan
  , binary ordInt opLessThanOrEq LessThanOrEqualTo
  , binary ordInt opGreaterThan GreaterThan
  , binary ordInt opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordNumber opLessThan LessThan
  , binary ordNumber opLessThanOrEq LessThanOrEqualTo
  , binary ordNumber opGreaterThan GreaterThan
  , binary ordNumber opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordString opLessThan LessThan
  , binary ordString opLessThanOrEq LessThanOrEqualTo
  , binary ordString opGreaterThan GreaterThan
  , binary ordString opGreaterThanOrEq GreaterThanOrEqualTo

  , binary heytingAlgebraBoolean opConj And
  , binary heytingAlgebraBoolean opDisj Or
  , unary  heytingAlgebraBoolean opNot Not

  , inlineNonClassFunction (isModFn (EC.dataFunction, C.apply)) $ \f x -> EApp f [x]
  , inlineNonClassFunction (isModFn (EC.dataFunction, C.applyFlipped)) $ \x f -> EApp f [x]
  ] ++ 
  [ fn | i <- [0..10], fn <- [ mkFn i, runFn i ] ] ++
  [ fn | i <- [1..10], fn <- [ mkEffFn i, runEffFn i ] ]
  where
  binary ::  (Text, PSString) -> (Text, PSString) -> BinaryOperator -> Erl -> Erl
  binary dict fns op = everywhereOnErl convert
    where
    convert :: Erl -> Erl
    convert (EApp fn [dict', x, y]) | isDict dict dict' && isUncurriedFn fns fn = EBinary op x y
    convert other = other

  unary ::  (Text, PSString) -> (Text, PSString) -> UnaryOperator -> Erl -> Erl
  unary dicts fns op = everywhereOnErl convert
    where
    convert :: Erl -> Erl
    convert (EApp (EApp fn [dict']) [x]) | isDict dicts dict' && isDict fns fn = EUnary op x
    convert other = other

  inlineNonClassFunction :: (Erl -> Bool) -> (Erl -> Erl -> Erl) -> Erl -> Erl
  inlineNonClassFunction p f = everywhereOnErl convert
    where
    convert :: Erl -> Erl
    convert (EApp (EApp op' [x]) [y]) | p op' = f x y
    convert other = other

  isModFn :: (Text, Text) -> Erl -> Bool
  isModFn = isFn

  mkFn :: Int -> Erl -> Erl
  mkFn = mkFn' EC.dataFunctionUncurried C.mkFn $ \args e -> EFunN Nothing args e

  mkEffFn :: Int -> Erl -> Erl
  mkEffFn = mkFn' EC.effectUncurried C.mkEffectFn $ \args e -> EFunN Nothing args (EApp e [])

  mkFn' :: Text -> Text -> ([Text] -> Erl -> Erl) -> Int -> Erl -> Erl
  mkFn' modName fnName res 0 = convert where
    convert :: Erl -> Erl
    convert (EApp mkFnN [EFun1 Nothing _ e]) | isNFn modName fnName 0 mkFnN = res [] e
    convert other = other
  mkFn' modName fnName res n = convert where
    convert :: Erl -> Erl
    convert (EApp mkFnN [ fn ])
      | isNFn modName fnName n mkFnN 
      , Just (args, e) <- collectArgs n [] fn = res args e
    convert other = other

    collectArgs :: Int -> [Text] -> Erl -> Maybe ([Text], Erl)
    collectArgs 1 acc (EFun1 Nothing arg e) | length acc == n - 1 = Just (reverse (arg : acc), e)
    collectArgs m acc (EFun1 Nothing arg e) = collectArgs (m - 1) (arg : acc) e
    collectArgs _ _   _ = Nothing

  runFn :: Int -> Erl -> Erl
  runFn = runFn' EC.dataFunctionUncurried C.runFn EApp

  runEffFn :: Int -> Erl -> Erl
  runEffFn = runFn' EC.effectUncurried C.runEffectFn $ \fn acc -> EFun0 Nothing (EApp fn acc)

  runFn' :: Text -> Text -> (Erl -> [Erl] -> Erl) -> Int -> Erl -> Erl
  runFn' modName runFnName res n = convert where
    convert :: Erl -> Erl
    convert e = fromMaybe e $ go e

    go :: Erl -> Maybe Erl
    go (EApp runFnN (fn : args))
      | isNFn modName runFnName n runFnN
      , length args == n
      = Just $ res (normaliseRef fn) args
    go _ = Nothing

    normaliseRef (EFunRef _ arity) | arity /= n = error "Should never see wrong arity here"
    normaliseRef (EFunRef fn _) = EAtomLiteral fn
    normaliseRef other = other

  isNFn :: Text -> Text -> Int -> Erl -> Bool
  isNFn expectMod prefix n fn
    | isUncurriedFn (expectMod, name) fn = True
    where name = mkString $ prefix <> T.pack (show n)
  isNFn _ _ _ _ = False
      

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
ordNumber = (C.dataOrd, C.ordNumber)

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
