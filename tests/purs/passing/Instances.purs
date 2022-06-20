module Instances where

import Prelude

import Data.Maybe (Maybe(..))

class MyShow a where
  myShow :: a -> String

instance showString :: MyShow String where
  myShow s = s

else instance showBoolean :: MyShow Boolean where
  myShow true = "true"
  myShow false = "false"

else instance showMaybe :: MyShow (Maybe a) where
  myShow _ = "maybe"

else instance showA :: MyShow a where
  myShow _ = "Invalid"

class Test2 a b

instance Test2 Int Int 
else instance Test2 (Maybe a) Int
else instance Test2 a String

class DeriveOutput :: Type -> Type -> Row Type -> Constraint
class DeriveOutput contexts payloads outputs | contexts payloads -> outputs

data CommonContext a b
data VData a
data VContext a

instance deriveOutput_CommonContext ::
  -- ( RL.RowToList payloadRow payloadRowL
  -- , MustMatch ctx payloadRowL ctxRow
  -- , DeriveOutputCommonContextRL ctx payloadRowL outputs
  -- , DeriveOutputBuilderCommonContext ctx ctxRow payloadRow payloadRowL outputs
  -- ) =>
  DeriveOutput (CommonContext ctx ctxRow) (VData payloadRow) outputs 

else instance deriveOutput_VariantContext ::
  -- ( RL.RowToList ctxRow ctxRowL
  -- , RL.RowToList payloadRow payloadRowL
  -- , DeriveOutputVariantContextRL ctxRowL payloadRowL outputs
  -- , DeriveOutputBuilderVariantContext ctxRow ctxRowL payloadRow payloadRowL outputs
  -- ) =>
  DeriveOutput (VContext ctxRow) (VData payloadRow) outputs 
else instance deriveOutput_UnifiedContext ::
  -- ( RL.RowToList payloadRow payloadRowL
  -- , DeriveOutputCommonContextRL ctx payloadRowL outputs
  -- , DeriveOutputBuilderUnifiedContext ctx payloadRow payloadRowL outputs
  -- ) =>
  DeriveOutput ctx (VData payloadRow) outputs 
