module TestCompiler where

-- Shamelessly stolen from purescript compiler tests

import Prelude ()
import Prelude.Compat

import qualified Language.PureScript as P
import Language.PureScript.Interactive.IO (readNodeProcessWithExitCode)

import Control.Arrow ((>>>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
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
import System.Directory
import System.Process hiding (cwd)



import Test.Hspec
import qualified System.FilePath.Glob as Glob
import TestUtils
import Data.Char (toLower)
import Debug.Trace (traceShowM)

spec :: Spec
spec = do
  passingTests
  optimizeTests

passingTests :: SpecWith () -- SupportModules
passingTests = do
  passingTestCases <- runIO $ getTestFiles "passing"

  describe "Passing examples" $
    beforeAllWith ((<$> createOutputFile logfile) . (,)) $
      forM_ passingTestCases $ \testPurs ->
        it ("'" <> takeFileName (getTestMain testPurs) <> "' should compile and run without error") $ \(support, outputFile) ->
          assertCompilesAndRuns testPurs outputFile

optimizeTests :: SpecWith ()
optimizeTests = do
  optimizeTestCases <- runIO $ getTestFiles "optimize"

  describe "Optimization examples" $
    forM_ optimizeTestCases $ \testPurs ->
      it ("'" <> takeFileName (getTestMain testPurs) <> "' should compile to expected output") $ \support ->
        assertCompilesToExpectedOutput testPurs


assertCompilesAndRuns
  :: [FilePath]
  -> Handle
  -> Expectation
assertCompilesAndRuns inputFiles outputFile = withCurrentDirectory "tests/support" $ do 
  -- Sorting the input files makes some messages (e.g., duplicate module) deterministic
  -- fs <- readInput (sort inputFiles)
  let mainModuleName = takeBaseName (getTestMain inputFiles)
  let inputFiles' = (\f -> ".." </> ".." </> f) <$> inputFiles
  result <- readProcessWithExitCode "spago" ([ "-q", "run", "-m", mainModuleName, "-p" ] ++ inputFiles') ""
  case result of
    (ExitSuccess, out, err)
      -- TODO build is writing to stderr, grep this out or something
      -- | not (null err) -> expectationFailure $ "Test wrote to stderr:\n\n" <> err
      | not (null out) && trim (last (lines out)) == "Done" -> hPutStr outputFile out
      | otherwise -> expectationFailure $ "Test did not finish with 'Done':\n\n" <> out
    (ExitFailure _, _, err) -> expectationFailure err

assertCompilesToExpectedOutput
  :: [FilePath]
  -> Expectation
assertCompilesToExpectedOutput inputFiles = do
  () <- compile inputFiles
  let mainModuleName@(first:rest) = takeBaseName (getTestMain inputFiles)
      moduleFilename = toLower first : rest
      
  let outputFilename = "tests" </> "support" </> "output" </> mainModuleName </> (moduleFilename <> "@ps.erl")
      goldenFilename = replaceExtension (getTestMain inputFiles) ".out.erl"
  goldenVsString goldenFilename (stripComments <$> BS.readFile outputFilename)

  where
  stripComments :: BS.ByteString -> BS.ByteString
  stripComments input = 
    let lines = Char8.split '\n' input
        isComment = BS.isPrefixOf "%"
    in
    BS.intercalate "\n" $ filter (not . isComment) lines

logfile :: FilePath
logfile = "purerl-tests.out"


getTestMain :: [FilePath] -> FilePath
getTestMain = minimumBy (compare `on` length)
