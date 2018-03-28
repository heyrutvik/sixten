module Command.Run where

import Data.Monoid
import Options.Applicative
import System.Process

import qualified Command.Compile as Compile
import qualified Command.Compile.Options as Compile
import Error
import qualified Processor.Result as Processor

data Options = Options
  { compileOptions :: Compile.Options
  , commandLineArguments :: Maybe String
  } deriving (Show)

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (helper <*> optionsParser)
  $ fullDesc
  <> progDesc "Run a Sixten program"
  <> header "sixten run"

optionsParser :: Parser Options
optionsParser = Options
  <$> Compile.optionsParser
  <*> optional (strOption
    $ long "args"
    <> metavar "ARGS"
    <> help "Command-line options passed to the Sixten program"
    )

run :: Options -> IO ()
run opts = Compile.compile (compileOptions opts) $ \res -> case res of
  Processor.Failure errs -> mapM_ printError errs
  Processor.Success (f, errs) -> do
    mapM_ printError errs
    callProcess f $ maybe [] words $ commandLineArguments opts

command :: ParserInfo (IO ())
command = run <$> optionsParserInfo
