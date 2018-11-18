module Driver where

import Protolude

import Backend.Target
import Driver.Query
import Driver.Rules
import Error

data Arguments = Arguments
  { sourceFiles :: NonEmpty FilePath
  , assemblyDir :: !FilePath
  , target :: !Target
  , logHandle :: !Handle
  , verbosity :: !Int
  , silentErrors :: !Bool
  } deriving (Eq, Show)

checkFiles
  :: Arguments
  -> IO [Error]
checkFiles = undefined

compileFiles
  :: Arguments
  -> FilePath
  -> IO [Error]
compileFiles args outputFile = undefined
