{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude

import           System.Environment (getArgs)
import qualified System.IO as IO
import qualified Options.Applicative as Opts
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import qualified Build
import qualified Command.REPL as REPL
import           Version (versionString)
import Control.Applicative

main :: IO ()
main = do
    IO.hSetEncoding IO.stdout IO.utf8
    IO.hSetEncoding IO.stderr IO.utf8
    IO.hSetBuffering IO.stdout IO.LineBuffering
    IO.hSetBuffering IO.stderr IO.LineBuffering


    cmd <- Opts.handleParseResult . execParserPure opts =<< getArgs
    cmd
  where
    opts        = Opts.info (versionInfo <*> optsParser) infoModList
    infoModList = Opts.fullDesc <> headerInfo <> footerInfo
    headerInfo  = Opts.progDesc "PureScript erlang backend (purerl)"
    footerInfo  = Opts.footerDoc (Just footer)

    footer =
      mconcat
        [ para $
            "For help using each individual command, run `purerl COMMAND --help`. " ++
            "For example, `purerl compile --help` displays options specific to the `compile` command."
        , Doc.hardline
        , Doc.hardline
        , Doc.text $ "purerl " ++ versionString
        ]

    para :: String -> Doc.Doc
    para = foldr (Doc.</>) Doc.empty . map Doc.text . words

    execParserPure :: Opts.ParserInfo a -> [String] -> Opts.ParserResult a
    execParserPure pinfo args = Opts.execParserPure Opts.defaultPrefs pinfo args

    versionInfo :: Opts.Parser (a -> a)
    versionInfo = Opts.abortOption (Opts.InfoMsg versionString) $
      Opts.long "version" <> Opts.help "Show the version number" <> Opts.hidden

    repl = Opts.flag' () $
      Opts.long "repl" <> Opts.help "Run REPL" <> Opts.hidden


    optsParser :: Opts.Parser (IO ())
    optsParser =
      Build.parser <|> (repl *> REPL.command)
