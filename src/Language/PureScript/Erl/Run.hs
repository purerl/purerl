{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BlockArguments #-}


module Language.PureScript.Erl.Run  where

import Prelude.Compat

import           System.IO
import           Control.Monad (when)
import qualified Language.PureScript as P
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe (catMaybes)
import           Language.PureScript.Erl.CodeGen.AST (Atom(..))
import           Language.PureScript.Erl.CodeGen.Common (runAtom, atomModuleName, ModuleType(..))
import           System.Process (readProcessWithExitCode)
import           System.Exit (exitFailure, ExitCode(..))
import           System.FilePath.Glob (glob)
import           Data.Traversable (for)
import           Language.PureScript.Erl.Make.Monad (catchDoesNotExist)
import           System.Directory (getModificationTime)
import           System.FilePath as FilePath

compileBeam :: Bool -> FilePath -> [ FilePath ] -> IO ()
compileBeam noisy ebin files = do
  files' <- catMaybes <$> for files \file -> do
      fileTS <- getModificationTime file
      let outFile = ebin </> FilePath.takeBaseName file <.> "beam"
      outputTS <- catchDoesNotExist $ getModificationTime outFile
      pure $ case outputTS of
        Just ts | ts >= fileTS -> Nothing
        _ -> Just file
  res@(_, out, _) <- readProcessWithExitCode "erlc" (["-o", ebin <> "/"] ++ files') ""
  exitOnFailure "Couldn't build with erlc" res
  when (noisy && out /= "") $ 
    putStrLn out

runProgram :: Text -> IO ()
runProgram runModule =
  case nameFromString runModule of
    Just (P.Qualified (P.ByModuleName mn') ident) -> do
      let erlName = runAtom $ Atom (Just $ atomModuleName mn' PureScriptModule) (P.runIdent ident)
      readProcessWithExitCode "mkdir" ["-p", "ebin"] [] >>=
        exitOnFailure "Couldn't create ebin directory"
      files <- glob "output/*/*.erl"
      when (files == []) $ do
        hPutStrLn stderr "Didn't find any .erl files in output directory"
        exitFailure 

      compileBeam True "ebin" files

      res@(_, out, _) <- readProcessWithExitCode "erl" [ "-pa", "ebin", "-noshell", "-eval", "io:setopts([{encoding,utf8}]), (" <> T.unpack erlName <> "())()", "-eval", "init:stop()" ] ""
      exitOnFailure "Error running erl" res
      putStr out
      pure ()
    _ -> do
      hPutStrLn stderr $ "Error parsing module: " <> T.unpack runModule
      exitFailure

  where

  nameFromString :: Text -> Maybe (P.Qualified P.Ident)
  nameFromString text =
    if length split < 2 || "." `elem` split then
      Nothing
    else 
      case splitAt (length split - 1) split of
        (modules, [ident]) -> 
          let mn = P.ModuleName $ T.intercalate "." modules
          in
          Just $ P.Qualified (P.ByModuleName mn) (P.Ident ident)
        _ -> Nothing
    where
    split = T.splitOn "." text

exitOnFailure :: Text -> (ExitCode, String, String) -> IO ()
exitOnFailure msg (code, out, err) =
  when (code /= ExitSuccess) $ do
    hPutStrLn stderr $ T.unpack msg
    hPutStrLn stderr out
    hPutStrLn stderr err
    exitFailure