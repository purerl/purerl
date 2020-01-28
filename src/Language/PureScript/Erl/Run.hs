{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}


module Language.PureScript.Erl.Run  where

import Prelude.Compat

import           System.IO
import           Control.Monad (when)
import qualified Language.PureScript as P
import           Data.Text (Text)
import qualified Data.Text as T
import           Language.PureScript.Erl.CodeGen.AST (Atom(..))
import           Language.PureScript.Erl.CodeGen.Common (runAtom, atomModuleName, ModuleType(..))
import           System.Process (readProcessWithExitCode)
import           System.Exit (exitFailure, ExitCode(..))
import           System.FilePath.Glob (glob)

runProgram :: Text -> IO ()
runProgram runModule =
  case nameFromString runModule of
    Just (P.Qualified (Just mn') ident) -> do
      let erlName = runAtom $ Atom (Just $ atomModuleName mn' PureScriptModule) (P.runIdent ident)
      readProcessWithExitCode "mkdir" ["-p", "ebin"] [] >>=
        exitOnFailure "Couldn't create ebin directory"
      files <- glob "output/*/*.erl"
      when (files == []) $ do
        hPutStrLn stderr "Didn't find any .erl files in output directory"
        exitFailure 
      readProcessWithExitCode "erlc" (["-o", "ebin/"] ++ files) "" >>=
        exitOnFailure "Couldn't build with erlc"
      res@(_, out, err) <- readProcessWithExitCode "erl" [ "-pa", "ebin", "-noshell", "-eval", "(" <> T.unpack erlName <> "())()", "-eval", "init:stop()" ] ""
      exitOnFailure "Error running erl" res
      putStrLn out
      pure ()
    _ -> do
      hPutStrLn stderr $ "Error parsing module: " <> T.unpack runModule
      exitFailure

  where
  exitOnFailure :: Text -> (ExitCode, String, String) -> IO ()
  exitOnFailure msg (code, out, err) =
    when (code /= ExitSuccess) $ do
      hPutStrLn stderr $ T.unpack msg
      hPutStrLn stderr out
      hPutStrLn stderr err
      exitFailure

  nameFromString :: Text -> Maybe (P.Qualified P.Ident)
  nameFromString text =
    if length split < 2 || "." `elem` split then
      Nothing
    else 
      case splitAt (length split - 1) split of
        (modules, [ident]) -> 
          let mn = P.ModuleName $ map P.ProperName modules
          in
          Just $ P.Qualified (Just mn) (P.Ident ident)
        _ -> Nothing
    where
    split = T.splitOn "." text