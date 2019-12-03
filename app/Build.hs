-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}


module Build (parser) where

import Prelude.Compat

import           Control.Exception (tryJust)
import           Control.Applicative
import           Control.Monad
-- import qualified Data.Aeson as A
-- import           Data.Bool (bool)
-- import qualified Data.ByteString.Lazy.UTF8 as LBU8
-- import           Data.List (catMaybes)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Map as M
-- import qualified Data.Set as S
-- import qualified Data.Text as T
-- import           Data.Traversable (for)
import qualified Language.PureScript as P
import qualified Language.PureScript.CoreFn as CoreFn
import qualified Language.PureScript.CoreFn.FromJSON as CoreFn
-- import qualified Language.PureScript.CST as CST
-- import           Language.PureScript.Errors.JSON
-- import           Language.PureScript.Make
import qualified Options.Applicative as Opts
-- import qualified System.Console.ANSI as ANSI
import           System.Exit (exitSuccess, exitFailure)
-- import           System.Directory (getCurrentDirectory)
import           System.FilePath.Glob (glob)
import           System.FilePath (joinPath)
import           System.IO (hPutStr, hPutStrLn, stderr)
import           System.IO.UTF8 (readUTF8FilesT)
import           System.IO.Error (tryIOError, isDoesNotExistError)
import Control.Monad.IO.Class
import           Data.Aeson as Aeson
import           Data.Aeson.Types (Parser, Value, listParser, parseMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.FilePath (replaceFileName, replaceExtension)
import           System.Directory (doesFileExist)
import qualified Language.PureScript.Erl.Make as Make
import           Control.Monad.Supply


data BuildOptions = BuildOptions
  { buildOutputDir    :: FilePath
  -- , pscmOpts         :: P.Options
  -- , pscmUsePrefix    :: Bool
  -- , pscmJSONErrors   :: Bool
  }



compile :: BuildOptions -> IO ()
compile BuildOptions{..} = do
  let -- externsGlob = joinPath [ buildOutputDir, "*", "externs.json" ]
      coreFnGlob = joinPath [ buildOutputDir, "*", "corefn.json" ]
  -- externs <- globWarningOnMisses warnFileTypeNotFound [externsGlob]
  corefn <- globWarningOnMisses warnFileTypeNotFound [coreFnGlob]
  when (null corefn) $ do
    hPutStr stderr $ unlines [ "purerl: No input files."
                             , "Usage: For basic information, try the `--help' option."
                             ]
    exitFailure
  (makeErrors, makeWarnings) <- P.runMake P.defaultOptions $ do
    modules <- forM corefn $ \corefn -> do
      let extern = replaceFileName corefn "externs.json"
      res <- readJSONFile corefn
      resExterns <- P.readExternsFile extern
      case res of
        Just cf -> case parseMaybe CoreFn.moduleFromJSON cf of
          Just (_version, module') -> do
            foreignFile <- liftIO $ inferForeignModule $ CoreFn.modulePath module'
            case resExterns of
              Just f -> do
                sourceTime <- max <$> P.getTimestamp corefn <*> P.getTimestamp extern
                foreignTime <- case foreignFile of 
                                Just ff -> max <$> P.getTimestamp ff <*> pure sourceTime
                                Nothing -> pure sourceTime
                pure $ Just (module', foreignFile, f, foreignTime)
              Nothing -> do
                liftIO $ putStrLn $ "Error parsing externs: " <> extern
                pure Nothing
          Nothing -> do
            liftIO $ putStrLn $ "Error parsing corefn: " <> corefn
            pure Nothing
        Nothing -> do
          liftIO $ putStrLn $ "Error parsing corefn: " <> corefn
          pure Nothing
    let modules' = catMaybes modules
        foreigns = M.fromList $ mapMaybe (\(m, fp, _, _) -> (CoreFn.moduleName m,) <$> fp) modules'
        env = foldr P.applyExternsFileToEnvironment P.initEnvironment $ map (\(_, _, e, _) -> e) modules'
        buildActions = Make.buildActions buildOutputDir env foreigns True

    forM_ modules' $ \(m, _, _, ts) -> do
      shouldBuild <- needsBuild buildActions ts $ CoreFn.moduleName m
      when shouldBuild $ do
        _ <- runSupplyT 0 $ Make.codegen buildActions m
        Make.ffiCodegen buildActions m

  -- printWarningsAndErrors (P.optionsVerboseErrors pscmOpts) pscmJSONErrors makeWarnings makeErrors
  exitSuccess

  where
  needsBuild buildActions ts m = do
    outputTs <- Make.getOutputTimestamp buildActions m
    pure $ case outputTs of
      Nothing -> True
      Just outTs | outTs < ts -> True
      _ -> False
    

  inferForeignModule :: MonadIO m => FilePath -> m (Maybe FilePath)
  inferForeignModule path = do
    let jsFile = replaceExtension path "erl"
    exists <- liftIO $ doesFileExist jsFile
    if exists
      then return (Just jsFile)
      else return Nothing

-- | Read a JSON file in the 'Make' monad, returning 'Nothing' if the file does
-- not exist or could not be parsed. Errors are captured using the 'MonadError'
-- instance.
readJSONFile :: FilePath -> P.Make (Maybe Value)
readJSONFile path =
  P.makeIO ("read JSON file: " <> T.pack path) $ do
    r <- catchDoesNotExist $ Aeson.decodeFileStrict path
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


buildOptions :: Opts.Parser BuildOptions
buildOptions = BuildOptions <$> outputDirectory
                            -- <*> options
                            -- <*> (not <$> noPrefix)
                            -- <*> jsonErrors

parser :: Opts.Parser (IO ())
parser = compile <$>  (buildOptions)
