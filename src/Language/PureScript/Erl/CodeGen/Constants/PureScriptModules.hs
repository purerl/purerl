module Language.PureScript.Erl.CodeGen.Constants.PureScriptModules

where

import Language.PureScript (ModuleName(..))

effect :: ModuleName
effect = ModuleName "Effect"

effectUncurried :: ModuleName
effectUncurried = ModuleName "Effect.Uncurried"

dataFunctionUncurried :: ModuleName
dataFunctionUncurried = ModuleName "Data.Function.Uncurried"

erlDataList :: ModuleName
erlDataList = ModuleName "Erl.Data.List"

erlDataMap :: ModuleName
erlDataMap = ModuleName "Erl.Data.Map"