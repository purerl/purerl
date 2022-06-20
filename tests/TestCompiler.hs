module TestCompiler where

-- Shamelessly stolen from purescript compiler tests

import Prelude ()
import Prelude.Compat

import qualified Language.PureScript as P
import Language.PureScript.Interactive.IO (readNodeProcessWithExitCode)

import Control.Arrow ((>>>))
import qualified Data.ByteString as BS
import Data.Function (on)
import Data.List (sort, stripPrefix, minimumBy, sortBy, groupBy)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


import Control.Monad

import System.Exit
import System.FilePath
import System.IO
import System.IO.UTF8 (readUTF8File)


import Test.Hspec
import qualified System.FilePath.Glob as Glob
import TestUtils (getTestFiles, createOutputFile, compile)

spec :: Spec
spec = do
  passingTests
--   optimizeTests

passingTests :: SpecWith () -- SupportModules
passingTests = do
  passingTestCases <- runIO $ getTestFiles "passing"

  describe "Passing examples" $
    beforeAllWith ((<$> createOutputFile logfile) . (,)) $
      forM_ passingTestCases $ \testPurs ->
        it ("'" <> takeFileName (getTestMain testPurs) <> "' should compile and run without error") $ \(support, outputFile) ->
          assertCompiles testPurs outputFile

-- optimizeTests :: SpecWith SupportModules
-- optimizeTests = do
--   optimizeTestCases <- runIO $ getTestFiles "optimize"

--   describe "Optimization examples" $
--     forM_ optimizeTestCases $ \testPurs ->
--       it ("'" <> takeFileName (getTestMain testPurs) <> "' should compile to expected output") $ \support ->
--         assertCompilesToExpectedOutput support testPurs



assertCompiles
  :: [FilePath]
  -> Handle
  -> Expectation
assertCompiles inputFiles outputFile = do
  () <- compile inputFiles
  pure ()
  -- case result of
  --   Left errs -> expectationFailure . P.prettyPrintMultipleErrors P.defaultPPEOptions $ errs
  --   Right _ -> do
  --     let entryPoint = modulesDir </> "index.js"
  --     writeFile entryPoint "import('./Main/index.js').then(({ main }) => main());"
  --     nodeResult <- readNodeProcessWithExitCode Nothing [entryPoint] ""
  --     hPutStrLn outputFile $ "\n" <> takeFileName (last inputFiles) <> ":"
  --     case nodeResult of
  --       Right (ExitSuccess, out, err)
  --         | not (null err) -> expectationFailure $ "Test wrote to stderr:\n\n" <> err
  --         | not (null out) && trim (last (lines out)) == "Done" -> hPutStr outputFile out
  --         | otherwise -> expectationFailure $ "Test did not finish with 'Done':\n\n" <> out
  --       Right (ExitFailure _, _, err) -> expectationFailure err
  --       Left err -> expectationFailure err

logfile :: FilePath
logfile = "purerl-tests.out"


getTestMain :: [FilePath] -> FilePath
getTestMain = minimumBy (compare `on` length)
