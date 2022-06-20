module Undef where

import Prelude
import Prelude

import Control.Monad.Free (Free)
import Data.Filterable (filter, filterMap)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Data.List (List, concat, nil)
import Erl.ModuleName (ModuleName, NativeModuleName, nativeModuleName)
import Erl.Test.EUnit (TestF, TestSet, collectTests)

foreign import findModuleNames :: String -> Effect (List ModuleName)
foreign import getExportedTests :: ModuleName -> NativeModuleName -> Effect (Maybe (Free TestF Unit))
foreign import filterTests :: (String -> Boolean) -> List TestSet -> List TestSet


-- findTests :: Effect _
-- findTests = do
--   -- pure nil
--   --   -- <#> filter moduleFilter
--   --   >>= 
--     traverse (const $ pure Nothing) nil
--     -- <#> filterMap identity
--     -- <#> map collectTests
--     -- <#> concat
--     -- <#> filterTests testFilter


main :: Effect Unit
main = do
  x <- traverse pure [ 1 ,2 ,3 ]
  pure unit