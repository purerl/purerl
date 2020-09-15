{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Command.REPL (command) where

import           Prelude ()
import           Prelude.Compat
import           Control.Applicative (many)
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.State.Strict (StateT, evalStateT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Data.Foldable (for_)
import qualified Language.PureScript as P
import qualified Language.PureScript.CST as CST
import           Language.PureScript.Erl.Interactive

import qualified Options.Applicative as Opts
import           System.Console.Haskeline
import           System.IO.UTF8 (readUTF8File)
import           System.Exit
import           System.Directory (doesFileExist, getCurrentDirectory, createDirectoryIfMissing)
import           System.FilePath ((</>))
import           System.FilePath.Glob (glob)
import           System.Process (readProcessWithExitCode)

import           Build as Build


-- | Command line options
data PSCiOptions = PSCiOptions
  { psciInputGlob         :: [String]
  }

inputFile :: Opts.Parser FilePath
inputFile = Opts.strArgument $
     Opts.metavar "FILES"
  <> Opts.help "Optional .purs files to load on start"

psciOptions :: Opts.Parser PSCiOptions
psciOptions = PSCiOptions <$> many inputFile

-- | Parses the input and returns either a command, or an error as a 'String'.
getCommand :: forall m. MonadException m => InputT m (Either String [Command])
getCommand = handleInterrupt (return (Right [])) $ do
  line <- withInterrupt $ getInputLine "> "
  case line of
    Nothing -> return (Right [QuitPSCi]) -- Ctrl-D when input is empty
    Just "" -> return (Right [])
    Just s  -> return (parseCommand s)

pasteMode :: forall m. MonadException m => InputT m (Either String [Command])
pasteMode =
    parseCommand <$> go []
  where
    go :: [String] -> InputT m String
    go ls = maybe (return . unlines $ reverse ls) (go . (:ls)) =<< getInputLine "… "

options :: Opts.Parser PSCiOptions
options = Opts.helper <*> psciOptions

-- | Get command line options and drop into the REPL
command :: Opts.Parser (IO ())
command = loop <$> options
  where
    loop :: PSCiOptions -> IO ()
    loop PSCiOptions{..} = do
        inputFiles <- concat <$> traverse glob psciInputGlob
        when (null inputFiles) . liftIO $ do
            putStr noInputMessage
            exitFailure

        e <- runExceptT $ do
          modules <- ExceptT (loadAllModules inputFiles)
          when (null modules) . liftIO $ do
            putStr noInputMessage
            exitFailure
          unless (supportModuleIsDefined (map (P.getModuleName . snd) modules)) . liftIO $ do
            putStr supportModuleMessage
            exitFailure
          (externs, _) <- ExceptT . runMake . make $ fmap CST.pureResult <$> modules
          return (modules, externs)

        case e of
          Left errs -> do
            pwd <- getCurrentDirectory
            putStrLn (P.prettyPrintMultipleErrors P.defaultPPEOptions {P.ppeRelativeDirectory = pwd} errs) >> exitFailure
          Right (modules, externs) -> do
            historyFilename <- getHistoryFilename
            let settings = defaultSettings { historyFile = Just historyFilename }
                initialState = updateLoadedExterns (const (zip (map snd modules) externs)) initialPSCiState
                config = PSCiConfig psciInputGlob
                runner = flip runReaderT config
                          . flip evalStateT initialState
                          . runInputTBehavior preferTerm (setComplete completion settings)

                handleCommand' :: state -> Command -> StateT PSCiState (ReaderT PSCiConfig IO) ()
                handleCommand' state = handleCommand (liftIO $ eval state) (liftIO (reload state)) (liftIO . putStrLn)

                go :: state -> InputT (StateT PSCiState (ReaderT PSCiConfig IO)) ()
                go state = do
                  c <- getCommand
                  case c of
                    Left err -> outputStrLn err >> go state
                    Right xs -> goExec xs
                  where
                  goExec :: [Command] -> InputT (StateT PSCiState (ReaderT PSCiConfig IO)) ()
                  goExec xs = case xs of
                    [] -> go state
                    (PasteLines : rest) -> do
                      c' <- pasteMode
                      case c' of
                        Left err -> outputStrLn err >> goExec rest
                        Right c'' -> handleCommandWithInterrupts state c'' >> goExec rest
                    (QuitPSCi : _) -> do
                      liftIO $ putStrLn "Quit command"
                      outputStrLn quitMessage
                      liftIO $ shutdown state
                    (c' : rest) -> handleCommandWithInterrupts state [c'] >> goExec rest

                loadUserConfig :: state -> StateT PSCiState (ReaderT PSCiConfig IO) ()
                loadUserConfig state = do
                  configFile <- (</> ".purs-repl") <$> liftIO getCurrentDirectory
                  exists <- liftIO $ doesFileExist configFile
                  when exists $ do
                    cf <- liftIO (readUTF8File configFile)
                    case parseDotFile configFile cf of
                      Left err -> liftIO (putStrLn err >> exitFailure)
                      Right cmds -> liftIO (putStrLn cf) >> for_ cmds (handleCommand' state)

                handleCommandWithInterrupts
                  :: state
                  -> [Command]
                  -> InputT (StateT PSCiState (ReaderT PSCiConfig IO)) ()
                handleCommandWithInterrupts state cmds = do
                  handleInterrupt (outputStrLn "Interrupted.")
                                  (withInterrupt (lift (for_ cmds (handleCommand' state))))

            putStrLn prologueMessage
            backendState <- setup
            runner (lift (loadUserConfig backendState) >> go backendState)

        return ()

    
    compileBeam :: Bool -> [String] -> IO ()
    compileBeam noisy files = do 
      result <- readProcessWithExitCode "erlc" (["-o", modulesDir <> "/ebin"] ++ files) ""
      when noisy $
        putStrResult result

    setup :: IO ()
    setup = do
      createDirectoryIfMissing True (modulesDir <> "/ebin")

      _ <- Build.compile' (Build.BuildOptions modulesDir Nothing False)
      files <- glob (modulesDir <> "*/*.erl")
      compileBeam True files

    eval :: state -> IO ()
    eval _ = do
      _ <- Build.compile' (Build.BuildOptions modulesDir Nothing True)
      compileBeam False [modulesDir <> "/$PSCI/$PSCI@ps.erl"]
      result <- readProcessWithExitCode "erl" ["-pa", modulesDir  <> "/ebin", "-noshell", "-eval", "('$PSCI@ps':'$main'())()", "-s", "erlang", "halt"] ""
      putStrResult result
      
    putStrResult :: (ExitCode, String, String) -> IO ()
    putStrResult result = 
      case result of
        (ExitSuccess, out, _) -> when (out /= "") $ putStrLn out
        (ExitFailure _, out, err) -> case err of
          "" -> do 
            putStrLn "Error"
            putStrLn out
          _ -> putStrLn err

    reload :: state -> IO ()
    reload _ = return ()

    shutdown :: state -> IO ()
    shutdown _ = return ()