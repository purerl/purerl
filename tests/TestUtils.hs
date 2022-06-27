module TestUtils where

import Prelude ()
import Prelude.Compat

import qualified Language.PureScript as P
import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.AST as AST
import qualified Language.PureScript.Names as N
import Language.PureScript.Interactive.IO (findNodeProcess)

import Control.Arrow ((***), (>>>))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Class (tell)
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Function (on)
import Data.List (sort, sortBy, stripPrefix, groupBy, find, intersperse, intercalate)
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (UTCTime(), diffUTCTime, getCurrentTime, nominalDay)
import Data.Tuple (swap)
import System.Directory
import System.Exit
import System.Environment (lookupEnv)
import System.FilePath
import System.IO.Error (isDoesNotExistError)
import System.IO.UTF8 (readUTF8FileT)
import System.Process hiding (cwd)
import qualified System.FilePath.Glob as Glob
import System.IO
import Test.Hspec

getTestFiles :: FilePath -> IO [[FilePath]]
getTestFiles testDir = do
  let dir = "tests" </> "purs" </> testDir
  getFiles dir <$> testGlob dir
  where
  -- A glob for all purs and js files within a test directory
  testGlob :: FilePath -> IO [FilePath]
  testGlob = Glob.globDir1 (Glob.compile "**/*.purs")
  -- Groups the test files so that a top-level file can have dependencies in a
  -- subdirectory of the same name. The inner tuple contains a list of the
  -- .purs files and the .js files for the test case.
  getFiles :: FilePath -> [FilePath] -> [[FilePath]]
  getFiles baseDir
    = map (filter ((== ".purs") . takeExtensions) . map (baseDir </>))
    . groupBy ((==) `on` extractPrefix)
    . sortBy (compare `on` extractPrefix)
    . map (makeRelative baseDir)
  -- Extracts the filename part of a .purs file, or if the file is in a
  -- subdirectory, the first part of that directory path.
  extractPrefix :: FilePath -> FilePath
  extractPrefix fp =
    let dir = takeDirectory fp
        ext = reverse ".purs"
    in if dir == "."
       then maybe fp reverse $ stripPrefix ext $ reverse fp
       else dir

buildSupportModules :: IO ()
buildSupportModules = withCurrentDirectory "tests/support" $ do
  callCommand "spago -q build --deps-only"

compile
  :: [FilePath]
  -> IO ()
compile inputFiles = withCurrentDirectory "tests/support" $ do
    -- Sorting the input files makes some messages (e.g., duplicate module) deterministic
  -- fs <- readInput (sort inputFiles)
  let inputFiles' = (\f -> ".." </> ".." </> f) <$> inputFiles
  result <- readProcessWithExitCode "spago" ([ "-q", "build", "-p" ] ++ intersperse "-p" inputFiles') ""
  case result of
    (ExitSuccess, out, err) -> pure () -- hPutStr outputFile out
    (ExitFailure _, _, err) -> expectationFailure err


createOutputFile :: FilePath -> IO Handle
createOutputFile logfileName = do
  tmp <- getTemporaryDirectory
  createDirectoryIfMissing False (tmp </> logpath)
  openFile (tmp </> logpath </> logfileName) WriteMode

updateSupportCode :: IO ()
updateSupportCode = withCurrentDirectory "tests/support" $ do
  let lastUpdatedFile = ".last_updated"
  skipUpdate <- fmap isJust . runMaybeT $ do
    -- We skip the update if: `.last_updated` exists,
    lastUpdated <- MaybeT $ getModificationTimeMaybe lastUpdatedFile

    -- ... and it was modified less than a day ago (no particular reason why
    -- "one day" specifically),
    now <- lift getCurrentTime
    guard $ now `diffUTCTime` lastUpdated < nominalDay

    -- ... and the needed directories exist,
    contents <- lift $ listDirectory "."
    guard $ ".spago" `elem` contents

    -- ... and everything else in `tests/support` is at least as old as
    -- `.last_updated`.
    modTimes <- lift $ traverse getModificationTime . filter (/= lastUpdatedFile) $ contents
    guard $ all (<= lastUpdated) modTimes

    pure ()

  unless skipUpdate $ do
    heading "Updating support code"
    callCommand "spago install"
    writeFile lastUpdatedFile ""
  where
  cannotFindNode :: String -> IO a
  cannotFindNode message = do
    hPutStrLn stderr message
    exitFailure

  getModificationTimeMaybe :: FilePath -> IO (Maybe UTCTime)
  getModificationTimeMaybe f = catch (Just <$> getModificationTime f) $ \case
    e | isDoesNotExistError e -> pure Nothing
      | otherwise             -> throw e

  heading msg = do
    putStrLn ""
    putStrLn $ replicate 79 '#'
    putStrLn $ "# " ++ msg
    putStrLn $ replicate 79 '#'
    putStrLn ""



-- | Assert that the contents of the provided file path match the result of the
-- provided action. If the "HSPEC_ACCEPT" environment variable is set, or if the
-- file does not already exist, we write the resulting ByteString out to the
-- provided file path instead. However, if the "CI" environment variable is
-- set, "HSPEC_ACCEPT" is ignored and we require that the file does exist with
-- the correct contents (see #3808). Based (very loosely) on the tasty-golden
-- package.
goldenVsString
  :: HasCallStack -- For expectationFailure; use the call site for better failure locations
  => FilePath
  -> IO ByteString
  -> Expectation
goldenVsString goldenFile testAction = do
  accept <- isJust <$> lookupEnv "HSPEC_ACCEPT"
  ci <- isJust <$> lookupEnv "CI"
  goldenContents <- tryJust (guard . isDoesNotExistError) (BS.readFile goldenFile)
  case goldenContents of
    Left () ->
      -- The golden file does not exist
      if ci
        then expectationFailure $ "Missing golden file: " ++ goldenFile
        else createOrReplaceGoldenFile

    Right _ | not ci && accept ->
      createOrReplaceGoldenFile

    Right expected -> do
      actual <- testAction
      if expected == actual
        then pure ()
        else expectationFailure $
          "Test output differed from '" ++ goldenFile ++ "'; got:\n" ++
          T.unpack (T.decodeUtf8With (\_ _ -> Just '\xFFFD') actual)
  where
  createOrReplaceGoldenFile = do
    testAction >>= BS.writeFile goldenFile
    pendingWith "Accepting new output"

logpath :: FilePath
logpath = "purescript-output"

trim :: String -> String
trim = dropWhile isSpace >>> reverse >>> dropWhile isSpace >>> reverse
