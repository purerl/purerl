module Language.PureScript.Erl.CodeGen.Constants where

import Data.String (IsString)
import Language.PureScript.PSString (PSString)

data EffectDictionaries = EffectDictionaries
  { edApplicativeDict :: PSString
  , edBindDict :: PSString
  , edMonadDict :: PSString
  , edWhile :: PSString
  , edUntil :: PSString
  , edFunctor :: PSString
  }

effDictionaries :: EffectDictionaries
effDictionaries = EffectDictionaries
  { edApplicativeDict = "applicativeEff"
  , edBindDict = "bindEff"
  , edMonadDict = "monadEff"
  , edWhile = "whileE"
  , edUntil = "untilE"
  , edFunctor = "functorEff"
  }

effectDictionaries :: EffectDictionaries
effectDictionaries = EffectDictionaries
  { edApplicativeDict = "applicativeEffect"
  , edBindDict = "bindEffect"
  , edMonadDict = "monadEffect"
  , edWhile = "whileE"
  , edUntil = "untilE"
  , edFunctor = "functorEffect"
  }


-- Modules

prim :: forall a. (IsString a) => a
prim = "prim@ps"

prelude :: forall a. (IsString a) => a
prelude = "prelude@ps"

dataArray :: forall a. (IsString a) => a
dataArray = "data_array@ps"

eff :: forall a. (IsString a) => a
eff = "control_monad_eff@ps"

effect :: forall a. (IsString a) => a
effect = "effect@ps"

controlApplicative :: forall a. (IsString a) => a
controlApplicative = "control_applicative@ps"

controlSemigroupoid :: forall a. (IsString a) => a
controlSemigroupoid = "control_semigroupoid@ps"

controlBind :: forall a. (IsString a) => a
controlBind = "control_bind@ps"

dataBounded :: forall a. (IsString a) => a
dataBounded = "data_bounded@ps"

dataSemigroup :: forall a. (IsString a) => a
dataSemigroup = "data_semigroup@ps"

dataHeytingAlgebra :: forall a. (IsString a) => a
dataHeytingAlgebra = "data_heytingAlgebra@ps"

dataEq :: forall a. (IsString a) => a
dataEq = "data_eq@ps"

dataOrd :: forall a. (IsString a) => a
dataOrd = "data_ord@ps"

dataSemiring :: forall a. (IsString a) => a
dataSemiring = "data_semiring@ps"

dataRing :: forall a. (IsString a) => a
dataRing = "data_ring@ps"

dataEuclideanRing :: forall a. (IsString a) => a
dataEuclideanRing = "data_euclideanRing@ps"

dataFunction :: forall a. (IsString a) => a
dataFunction = "data_function@ps"

dataFunctor :: forall a. (IsString a) => a
dataFunctor = "data_functor@ps"

dataFunctionUncurried :: forall a. (IsString a) => a
dataFunctionUncurried = "data_function_uncurried@ps"

effectUncurried :: forall a. (IsString a) => a
effectUncurried = "effect_uncurried@ps"

dataUnit :: forall a. (IsString a) => a
dataUnit = "data_unit@ps"

dataInt :: forall a. (IsString a) => a
dataInt = "data_int@ps"

dataIntBits :: forall a. (IsString a) => a
dataIntBits = "data_int_bits@ps"

erlAtom :: forall a. (IsString a) => a
erlAtom = "erl_atom@ps"

erlDataMap :: forall a. (IsString a) => a
erlDataMap = "erl_data_map@ps"

erlDataList :: forall a. (IsString a) => a
erlDataList = "erl_data_list@ps"

erlDataListTypes :: forall a. (IsString a) => a
erlDataListTypes = "erl_data_list_types@ps"

erlDataTuple :: forall a. (IsString a) => a
erlDataTuple = "erl_data_tuple@ps"

unsafeCoerceMod :: forall a. (IsString a) => a
unsafeCoerceMod = "unsafe_coerce@ps"

safeCoerceMod :: forall a. (IsString a) => a
safeCoerceMod = "safe_coerce@ps"

dataNewtype :: forall a. (IsString a) => a
dataNewtype = "data_newtype@ps"

dataMaybe :: forall a. (IsString a) => a
dataMaybe = "data_maybe@ps"

dataEither :: forall a. (IsString a) => a
dataEither = "data_either@ps"

-- Value names

toNumber :: forall a. (IsString a) => a
toNumber = "toNumber"

atom :: forall a. (IsString a) => a
atom = "atom"

void :: forall a. (IsString a) => a
void = "void"

unit :: forall a. (IsString a) => a
unit = "unit"

empty :: forall a. (IsString a) => a
empty = "empty"

nil :: forall a. (IsString a) => a
nil = "nil"

cons :: forall a. (IsString a) => a
cons = "cons"

null :: forall a. (IsString a) => a
null = "null"

min :: forall a. (IsString a) => a
min = "min"

max :: forall a. (IsString a) => a
max = "max"

singleton :: forall a. (IsString a) => a
singleton = "singleton"

unsafeCoerce :: forall a. (IsString a) => a
unsafeCoerce = "unsafeCoerce"

coerce :: forall a. (IsString a) => a
coerce = "coerce"

wrap :: forall a. (IsString a) => a
wrap = "wrap"

unwrap :: forall a. (IsString a) => a
unwrap = "unwrap"

over :: forall a. (IsString a) => a
over = "over"

over2 :: forall a. (IsString a) => a
over2 = "over2"

tuple :: forall a. (IsString a) => a
tuple = "tuple"

maybe :: forall a. (IsString a) => a
maybe = "maybe"

maybe' :: forall a. (IsString a) => a
maybe' = "maybe'"

fromMaybe :: forall a. (IsString a) => a
fromMaybe = "fromMaybe"

either :: forall a. (IsString a) => a
either = "either"

fromLeft :: forall a. (IsString a) => a
fromLeft = "fromLeft"

fromRight :: forall a. (IsString a) => a
fromRight = "fromRight"

-- Typeclass Dictionary names

euclideanRingInt :: forall a. (IsString a) => a
euclideanRingInt = "euclideanRingInt"

semigroupList :: forall a. (IsString a) => a
semigroupList = "semigroupList"

discardUnit :: forall a. (IsString a) => a
discardUnit = "discardUnit"
