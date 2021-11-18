{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}

module Build (parser, compile, compile', BuildOptions(..)) where

import Prelude
import           Control.Exception (tryJust)
import Control.Monad ( guard, join, unless, forM, when )
import           Data.Maybe (catMaybes, mapMaybe, fromMaybe, isNothing, maybeToList)
import           Data.Either (isLeft)
import           Protolude (hush, (&))
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
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Aeson as Aeson
    ( Value(String, Object), decodeFileStrict', FromJSON, ToJSON )
import           Data.Aeson.Types (parseMaybe)
import qualified Data.Text as T
import           System.FilePath (replaceFileName, replaceExtension, joinPath)
import qualified System.FilePath as FP
import           System.Directory (doesFileExist, getCurrentDirectory)
import qualified Language.PureScript.Erl.Make as Make
import qualified Language.PureScript.Erl.Make.Monad as MM
import Language.PureScript.Erl.Errors as E
    ( MultipleErrors,
      defaultPPEOptions,
      nonEmpty,
      prettyPrintMultipleErrors,
      prettyPrintMultipleWarnings,
      Level(Error, Warning),
      PPEOptions(ppeCodeColor, ppeFull, ppeRelativeDirectory) )
import Language.PureScript.Erl.Errors.JSON
    ( toJSONErrors, JSONResult(JSONResult) )
import Control.Monad.Supply ( runSupplyT )
import           Language.PureScript.Erl.Run (runProgram)
import Data.Time.Clock (UTCTime)

import qualified Data.HashMap.Strict as HashMap
import GHC.Generics (Generic)


import Debug.Trace
import Language.PureScript.Erl.CodeGen (buildCodegenEnvironment)
import Control.Monad.Base (MonadBase(liftBase))
import qualified Language.PureScript.Make.Cache as Cache
import Data.Bifunctor (first)
import Data.Traversable (for)

data BuildOptions = BuildOptions
  { buildOutputDir    :: FilePath
  , buildRun          :: Maybe String
  , buildQuiet        :: Bool
  , buildChecked      :: Bool
  }

data ModResult = ModResult
  { moduleName :: P.ModuleName
  , modulePath :: FilePath
  , coreFn :: Maybe Value
  , foreignFile :: Maybe FilePath
  , externs :: MM.Make (Maybe P.ExternsFile)
  -- , inputTimestamp :: UTCTime
  , newCacheInfo :: Map FilePath (UTCTime, MM.Make Cache.ContentHash)
  , oldCacheInfo :: Maybe FileCacheInfo
  }

compile :: BuildOptions -> IO ()
compile opts = do
  _ <- compile' opts
  exitSuccess

compile' :: BuildOptions -> IO (Maybe [CoreFn.Module CoreFn.Ann])
compile' BuildOptions{..} = do
  let coreFnGlob = joinPath [ buildOutputDir, "*", "corefn.json" ]
      cacheDbFile = joinPath [ buildOutputDir, "purerl-cache-db.json" ]
  corefnFiles <- globWarningOnMisses warnFileTypeNotFound [coreFnGlob]
  when (null corefnFiles) $ do
    hPutStr stderr $ unlines [ "purerl: No input files."
                             , "Usage: For basic information, try the `--help' option."
                             ]
    exitFailure
  (makeErrors, makeWarnings) <- MM.runMake P.defaultOptions $ do
    cache :: CacheDb <- fromMaybe M.empty <$> MM.readJSONFile cacheDbFile
    modules <- forM corefnFiles $ \corefn -> do
      let extern = replaceFileName corefn "externs.cbor"
          modStr = last $ FP.splitPath $ FP.takeDirectory corefn
          moduleName' = P.ModuleName $ T.pack modStr

      -- Aeson JSON handling is lazy, but if we evaluate the outer Maybe Value we incur the
      -- cost of constructing the big top-level object even if we don't look at it
      res <- readJSONFile corefn
      let readExterns = do
            resExterns <- MM.readExternsFile extern
            case resExterns of
              Just resExterns' -> do
                unless (P.externsIsCurrentVersion resExterns') $
                  liftIO $ hPutStrLn stderr $ "Found externs for wrong compiler version (continuing anyway): " <> extern
                pure $ Just resExterns'
              Nothing -> do
                liftIO $ hPutStrLn stderr $ "Error parsing externs: " <> extern
                pure Nothing

      let fromCorefn = case res of
                        Just res' -> do
                          let Just sourceFile = getModulePath res'
                          exists <- liftIO $ doesFileExist sourceFile
                          unless exists $ liftIO do
                            -- TODO We have to care about extraneous stuff in output/ because the set of source modules is implicit
                            -- This could be obviated by spago passing these on command line, in which case we could simply not build these things
                            -- TODO Consider batching up these errors - though probably the response to this is nuking output/ which will solve N of them.
                            hPutStrLn stderr $ "Source file " <> sourceFile <> " no longer exists for input " <> corefn
                            exitFailure
                          (foreignFile, newCacheInfo) <- getCacheInfo corefn extern sourceFile
                          pure $ Just $ ModResult moduleName' sourceFile res foreignFile readExterns newCacheInfo Nothing
                        Nothing -> do
                          liftIO $ hPutStrLn stderr $ "Error parsing corefn: " <> corefn
                          pure Nothing
          fromCache = M.lookup moduleName' cache & maybe (pure Nothing)
            \CacheInfo { sourceFile, cacheInfo = oldCacheInfo } -> do
                do
                  exists <- liftIO $ doesFileExist sourceFile
                  if exists then do
                    (foreignFile, newCacheInfo) <- getCacheInfo corefn extern sourceFile
                    pure $ Just $ ModResult moduleName' sourceFile res foreignFile readExterns newCacheInfo (Just oldCacheInfo)
                  else do
                    pure Nothing
      resCfn <- fromCache
      modRes <- maybe fromCorefn (pure . Just) resCfn

      for modRes \rr -> do
        (_cacheInfo, nb) <- needsBuild rr
        -- Source path might have changed, just start again as if we didn't find an entry in the cache
        -- We don't do this in the first place because it avoids parsing corefn for unchanged files
        -- (key point, if the source file moves then the corefn will have changed).
        -- While if we're needing to build, that will happen down the line anyway.
        if nb then do
          fromCorefn >>=
            maybe (liftIO exitFailure) pure
        else
          pure rr

    modules' <-case sequence modules of
      Just m' -> pure m'
      Nothing ->
        liftIO $ do
          hPutStrLn stderr "Exiting due to bad corefn"
          exitFailure

    let buildActions = Make.buildActions buildOutputDir foreigns True buildChecked
        foreigns = M.fromList $ mapMaybe (\ModResult{ moduleName, foreignFile } -> (moduleName,) <$> foreignFile) modules'

    -- TODO now we are asking needsBuild twice
    buildInfo <- traverse (\m -> (m,) <$> needsBuild m) modules'
    let needToBuild = mapMaybe (\(res, (_, ntb)) -> if ntb then Just res else Nothing) buildInfo
    let newCache :: CacheDb = M.fromList $ map (\(ModResult { moduleName, modulePath }, (ci, _)) -> (moduleName, CacheInfo modulePath ci) ) buildInfo
    MM.writeJSONFile cacheDbFile newCache

    if null needToBuild then do
      pure []
    else do
      externsFiles <- traverse externs modules'
      when (any isNothing externsFiles) $ liftIO do
        hPutStrLn stderr "Exiting due to externs error"
        exitFailure

      let env = buildCodegenEnvironment $ foldr P.applyExternsFileToEnvironment P.initEnvironment (catMaybes externsFiles)

      res :: [Either P.ModuleName (CoreFn.Module CoreFn.Ann)]
          <- forM needToBuild \ModResult{ moduleName, coreFn } -> do
        unless buildQuiet $
          liftIO $ hPutStrLn stderr $ "Building " <> T.unpack (P.runModuleName moduleName)
        case coreFn of
          Just coreFn'
            | Just (_version, module') <- parseMaybe CoreFn.moduleFromJSON coreFn' -> do
            _ <- runSupplyT 0 $ Make.codegen buildActions env module'
            Make.ffiCodegen buildActions module'
            pure $ Right module'
          _ -> do
            liftIO $ hPutStrLn stderr $ "Error parsing corefn: " <> T.unpack (P.runModuleName moduleName)
            pure $ Left moduleName

      liftIO $ when (any isLeft res) do
        traceShowM res
        hPutStrLn stderr "Exiting due to corefn error"
        exitFailure

      pure $ catMaybes $ hush <$> res

  printWarningsAndErrors False False makeWarnings makeErrors

  case buildRun of
    Nothing -> pure ()
    Just runModule -> runProgram $ T.pack runModule

  pure $ either (const Nothing) Just makeErrors

  where

  needsBuild :: ModResult -> MM.Make (FileCacheInfo, Bool)
  needsBuild ModResult { oldCacheInfo, newCacheInfo, moduleName } = do
    -- outputTs <- Make.getOutputTimestamp buildActions mn
    cwd <- liftBase getCurrentDirectory
    let wrapCI = M.singleton moduleName (Cache.CacheInfo $ fromMaybe M.empty oldCacheInfo)
    (cacheInfo, isUpToDate) <- first Cache.unCacheInfo <$> Cache.checkChanged wrapCI moduleName cwd newCacheInfo
    pure (cacheInfo, not isUpToDate)


  -- | Arguments: verbose, use JSON, warnings, errors
  printWarningsAndErrors :: forall a. Bool -> Bool -> MultipleErrors -> Either MultipleErrors a -> IO ()
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

  getModulePath :: Value -> Maybe FilePath
  getModulePath corefn =
    case corefn of
      Object o | Just (String filename) <- HashMap.lookup "modulePath" o -> Just $ T.unpack filename
      _ -> Nothing


  getCacheInfo :: FilePath -> FilePath -> FilePath -> MM.Make (Maybe FilePath, Map FilePath (UTCTime, MM.Make Cache.ContentHash))
  getCacheInfo corefn extern sourceFile = do
    let getInfo fp = do
          ts <- MM.getTimestamp fp
          pure (ts, MM.hashFile fp)
    foreignFile <- liftIO $ inferForeignModule' sourceFile
    newCacheInfo <- M.fromList <$> traverse (\fp -> (fp,) <$> getInfo fp) (corefn : extern : maybeToList foreignFile)
    pure (foreignFile, newCacheInfo)

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

quiet :: Opts.Parser Bool
quiet = Opts.flag False True $
  Opts.short 'q'
  <> Opts.long "quiet"
  <> Opts.help "Print less output"

checked :: Opts.Parser Bool
checked = Opts.flag False True $
  Opts.long "checked"
  <> Opts.help "Generate wrapper modules with run-time type-checking of function arguments"


buildOptions :: Opts.Parser BuildOptions
buildOptions = BuildOptions <$> outputDirectory
                            <*> run
                            <*> quiet
                            <*> checked

parser :: Opts.Parser (IO ())
parser = compile <$> buildOptions

type CacheDb = Map P.ModuleName CacheInfo

data CacheInfo = CacheInfo
  { sourceFile :: FilePath
  , cacheInfo :: FileCacheInfo
  }
  deriving (Generic, Show)

instance ToJSON CacheInfo where

instance FromJSON CacheInfo where

type FileCacheInfo = Map FilePath (UTCTime, Cache.ContentHash)