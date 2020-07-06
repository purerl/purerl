{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}



module Build (parser) where

import           Control.Exception (tryJust)
import           Control.Monad
import           Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Language.PureScript as P
import qualified Language.PureScript.CoreFn as CoreFn
import qualified Language.PureScript.CoreFn.FromJSON as CoreFn
import qualified Options.Applicative as Opts
import qualified System.Console.ANSI as ANSI
import qualified Data.Aeson as A
import           Data.Bool (bool)
import           System.Exit (exitSuccess, exitFailure)
import           System.FilePath.Glob (glob)
import           System.IO (hPutStr, hPutStrLn, stderr)
import           System.IO.Error (isDoesNotExistError)
import qualified Data.ByteString.Lazy.UTF8 as LBU8
import Control.Monad.IO.Class
import           Data.Aeson as Aeson
import           Data.Aeson.Types (Value, parseMaybe)
import qualified Data.Text as T
import           System.FilePath (replaceFileName, replaceExtension, joinPath)
import qualified System.FilePath as FP
import           System.Directory (doesFileExist, getCurrentDirectory)
import qualified Language.PureScript.Erl.Make as Make
import qualified Language.PureScript.Erl.Make.Monad as MM
import           Language.PureScript.Erl.Errors as E
import           Language.PureScript.Erl.Errors.JSON
import           Control.Monad.Supply
import           Language.PureScript.Erl.Run (runProgram)
import           Control.Monad.Trans.Class (MonadTrans(..))
import Data.Time.Clock (UTCTime)

import qualified Data.HashMap.Strict as HashMap
import Text.Printf
import qualified Data.Vector as Vector
import GHC.Generics

data BuildOptions = BuildOptions
  { buildOutputDir    :: FilePath
  , buildRun          :: Maybe String
  }

data ModResult = ModResult
  { moduleName :: P.ModuleName
  , modulePath :: FilePath
  , coreFn :: Maybe Value
  , foreignFile :: Maybe FilePath
  , externs :: P.ExternsFile
  , inputTimestamp :: UTCTime
  }

compile :: BuildOptions -> IO ()
compile BuildOptions{..} = do
  let coreFnGlob = joinPath [ buildOutputDir, "*", "corefn.json" ]
      cacheDbFile = joinPath [ buildOutputDir, "purerl-cache-db.json" ]
  corefnFiles <- globWarningOnMisses warnFileTypeNotFound [coreFnGlob]
  when (null corefnFiles) $ do
    hPutStr stderr $ unlines [ "purerl: No input files."
                             , "Usage: For basic information, try the `--help' option."
                             ]
    exitFailure
  (makeErrors, makeWarnings) <- MM.runMake P.defaultOptions $ do
    cache <- fromMaybe M.empty <$> MM.readJSONFile cacheDbFile
    let z :: CacheDb
        z  = cache
    modules <- forM corefnFiles $ \corefn -> do
      let extern = replaceFileName corefn "externs.json"
      let modStr = last $ FP.splitPath $ FP.takeDirectory corefn
      let moduleName' = P.ModuleName $ P.ProperName <$> T.splitOn "." (T.pack modStr)

      -- Aeson JSON handling is lazy, but if we evaluate the outer Maybe Value we incur the
      -- cost of constructing the big top-level object even if we don't look at it
      res <- readJSONFile corefn
      resExterns <- MM.readJSONFile extern

      case resExterns of
        Just resExterns' -> do
          when (not $ P.externsIsCurrentVersion resExterns') $
            liftIO $ hPutStrLn stderr $ "Found externs for wrong compiler version (continuing anyway): " <> extern
          let fromCorefn = case res of
                            Just res' -> do
                              let Just modulePath = getModulePath res'
                              foreignFile <- liftIO $ inferForeignModule' modulePath
                              inputTime <- latestInputTimestamp corefn extern foreignFile
                              pure $ Just $ ModResult moduleName' modulePath res foreignFile resExterns' inputTime
                            Nothing -> do
                              liftIO $ hPutStrLn stderr $ "Error parsing corefn: " <> corefn
                              pure Nothing
          case M.lookup moduleName' cache of
            Just (CacheInfo { sourceFile = sourceFile }) -> do
              exists <- liftIO $ doesFileExist sourceFile
              if exists then do
                foreignFile <- liftIO $ inferForeignModule' sourceFile
                inputTime <- latestInputTimestamp corefn extern foreignFile
                pure $ Just $ ModResult moduleName' sourceFile res foreignFile resExterns' inputTime
              else
                fromCorefn
            Nothing -> fromCorefn
              
        Nothing -> do
          liftIO $ hPutStrLn stderr $ "Error parsing externs: " <> extern
          pure Nothing
      
    let modules' = catMaybes modules
        foreigns = M.fromList $ mapMaybe (\ModResult{ moduleName, foreignFile} -> (moduleName,) <$> foreignFile) modules'
        env = foldr P.applyExternsFileToEnvironment P.initEnvironment $ map externs modules'
        buildActions = Make.buildActions buildOutputDir env foreigns True

    let newCache :: CacheDb
        newCache = M.fromList $ map (\ModResult { moduleName, modulePath} -> (moduleName, CacheInfo modulePath M.empty)) modules'
    MM.writeJSONFile cacheDbFile newCache

    forM_ modules' $ \ModResult{ moduleName, coreFn, inputTimestamp} -> do
      shouldBuild <- needsBuild buildActions inputTimestamp moduleName
      when shouldBuild $ do
        liftIO $ hPutStrLn stderr $ "Building " <> T.unpack (P.runModuleName moduleName)
        case coreFn of
          Just coreFn'
            | Just (_version, module') <- parseMaybe CoreFn.moduleFromJSON coreFn' -> do
            _ <- runSupplyT 0 $ Make.codegen buildActions module'
            Make.ffiCodegen buildActions module'
          Nothing -> do
            liftIO $ hPutStrLn stderr $ "Error parsing corefn: " <> T.unpack (P.runModuleName moduleName)
            pure ()


  printWarningsAndErrors False False makeWarnings makeErrors 

  case buildRun of
    Nothing -> pure ()
    Just runModule -> runProgram $ T.pack runModule
  
  exitSuccess
  where

  latestInputTimestamp corefn extern foreignFile = do
    sourceTime <- max <$> MM.getTimestamp corefn <*> MM.getTimestamp extern
    case foreignFile of 
      Just ff -> max <$> MM.getTimestamp ff <*> pure sourceTime
      Nothing -> pure sourceTime
          

  needsBuild buildActions ts m = do
    outputTs <- Make.getOutputTimestamp buildActions m
    pure $ case outputTs of
      Nothing -> True
      Just outTs | outTs < ts -> True
      _ -> False
    

  -- | Arguments: verbose, use JSON, warnings, errors
  printWarningsAndErrors :: Bool -> Bool -> MultipleErrors -> Either MultipleErrors a -> IO ()
  printWarningsAndErrors verbose False warnings errors = do
    pwd <- getCurrentDirectory
    cc <- bool Nothing (Just P.defaultCodeColor) <$> ANSI.hSupportsANSI stderr
    let ppeOpts = E.defaultPPEOptions { E.ppeCodeColor = cc, E.ppeFull = verbose, E.ppeRelativeDirectory = pwd }
    when (E.nonEmpty warnings) $
      hPutStrLn stderr (E.prettyPrintMultipleWarnings ppeOpts warnings)
    case errors of
      Left errs -> do
        hPutStrLn stderr (E.prettyPrintMultipleErrors ppeOpts errs)
        exitFailure
      Right _ -> return ()
  printWarningsAndErrors verbose True warnings errors = do
    hPutStrLn stderr . LBU8.toString . A.encode $
      JSONResult (toJSONErrors verbose E.Warning warnings)
                (either (toJSONErrors verbose E.Error) (const []) errors)
    either (const exitFailure) (const (return ())) errors

  getModuleName ::  Value -> Maybe P.ModuleName
  getModuleName corefn =
    case corefn of
      Object o | Just (Array filenames) <- HashMap.lookup "moduleName" o -> 
        Just $ P.ModuleName $ Vector.toList $ (\(String s) -> P.ProperName s) <$> filenames
      _ -> Nothing

  getModulePath :: Value -> Maybe FilePath
  getModulePath corefn =
    case corefn of
      Object o | Just (String filename) <- HashMap.lookup "modulePath" o -> Just $ T.unpack filename
      _ -> Nothing

  inferForeignModule' :: MonadIO m => FilePath -> m (Maybe FilePath)
  inferForeignModule' path = do
    let jsFile = replaceExtension path "erl"
    exists <- liftIO $ doesFileExist jsFile
    if exists
      then return (Just jsFile)
      else return Nothing

-- | Read a JSON file in the 'Make' monad, returning 'Nothing' if the file does
-- not exist or could not be parsed. Errors are captured using the 'MonadError'
-- instance.
readJSONFile :: FilePath -> MM.Make (Maybe Value)
readJSONFile path =
  MM.makeIO ("read JSON file: " <> T.pack path) $ do
    r <- catchDoesNotExist $ Aeson.decodeFileStrict' path
    return $ join r

-- | If the provided action threw an 'isDoesNotExist' error, catch it and
-- return Nothing. Otherwise return Just the result of the inner action.
catchDoesNotExist :: IO a -> IO (Maybe a)
catchDoesNotExist inner = do
  r <- tryJust (guard . isDoesNotExistError) inner
  case r of
    Left () ->
      return Nothing
    Right x ->
      return (Just x)

tdiff before after = ((fromIntegral (after - before)) / (10^12)) :: Double

warnFileTypeNotFound :: String -> IO ()
warnFileTypeNotFound = hPutStrLn stderr . ("purs compile: No files found using pattern: " ++)

globWarningOnMisses :: (String -> IO ()) -> [FilePath] -> IO [FilePath]
globWarningOnMisses warn = concatMapM globWithWarning
  where
  globWithWarning pattern' = do
    paths <- glob pattern'
    when (null paths) $ warn pattern'
    return paths
  concatMapM f = fmap concat . mapM f

outputDirectory :: Opts.Parser FilePath
outputDirectory = Opts.strOption $
      Opts.short 'o'
  <> Opts.long "output"
  <> Opts.value "output"
  <> Opts.showDefault
  <> Opts.help "The output directory"

run :: Opts.Parser (Maybe String)
run = Opts.optional $ Opts.strOption $
    Opts.long "run"
    <> Opts.help "Run the given function"


buildOptions :: Opts.Parser BuildOptions
buildOptions = BuildOptions <$> outputDirectory
                            <*> run
                            -- <*> (not <$> noPrefix)
                            -- <*> jsonErrors

parser :: Opts.Parser (IO ())
parser = compile <$> buildOptions



type CacheDb = Map P.ModuleName CacheInfo

data CacheInfo = CacheInfo
  { sourceFile :: FilePath
  , cacheInfo :: Map FilePath UTCTime }
  deriving (Generic, Show)

instance ToJSON CacheInfo where

instance FromJSON CacheInfo where