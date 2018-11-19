{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Command.Check where

import Protolude

import Options.Applicative as Options
import Util

import qualified Backend.Target as Target
import Command.Check.Options
import qualified Driver

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (helper <*> optionsParser)
  $ fullDesc
  <> progDesc "Type check a Sixten program"
  <> header "sixten check"

optionsParser :: Parser Options
optionsParser = Options
  <$> nonEmptySome (strArgument
    $ metavar "FILES..."
    <> help "Input source FILES"
    <> action "file"
    )
  <*> Options.option auto
    (long "verbose"
    <> short 'v'
    <> metavar "LEVEL"
    <> help "Set the verbosity level to LEVEL"
    <> value 0
    <> completeWith ["0", "10", "20", "30", "40"]
    )
  <*> optional (strOption
    $ long "log-file"
    <> metavar "FILE"
    <> help "Write logs to FILE instead of standard output"
    <> action "file"
    )

check
  :: Options
  -> IO ()
check opts = withLogHandle (logFile opts) $ \logHandle -> do
  errors <- Driver.checkFiles Driver.Arguments
    { Driver.sourceFiles = inputFiles opts
    , Driver.assemblyDir = ""
    , Driver.target = Target.defaultTarget
    , Driver.logHandle = logHandle
    , Driver.verbosity = verbosity opts
    , Driver.silentErrors = False
    }
  case errors of
    [] -> do
      putText "Type checking completed successfully"
      exitSuccess
    _ -> do
      putText "Type checking failed"
      exitFailure
  where
    withLogHandle Nothing k = k stdout
    withLogHandle (Just file) k = Util.withFile file WriteMode k

command :: ParserInfo (IO ())
command = Command.Check.check <$> optionsParserInfo
