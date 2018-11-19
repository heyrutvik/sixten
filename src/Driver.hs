module Driver where

import Protolude

import Backend.Target
import Driver.Query
import Driver.Rules
import Error
import Syntax
import qualified Syntax.Core as Core
import Syntax.Extern

data Arguments = Arguments
  { sourceFiles :: NonEmpty FilePath
  , assemblyDir :: !FilePath
  , target :: !Target
  , logHandle :: !Handle
  , verbosity :: !Int
  , silentErrors :: !Bool
  } deriving (Eq, Show)

data Status
  = Failure
  | Success
  deriving (Eq, Show)

checkFiles
  :: Arguments
  -> IO [Error]
checkFiles = undefined

checkVirtualFile
  :: FilePath
  -> Text
  -> IO ([[(QName, SourceLoc, ClosedDefinition Core.Expr, Biclosed Core.Expr)]], [Error])
checkVirtualFile = undefined

data CompileResult = CompileResult
  { externFiles :: [(Language, FilePath)]
  , llFiles :: [FilePath]
  }

compileFiles
  :: FilePath
  -> Arguments
  -> IO (Maybe CompileResult, [Error])
compileFiles outputFile args = undefined
