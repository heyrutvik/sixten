module Syntax.ModuleHeader where

import Protolude hiding (moduleName)

import Data.HashSet(HashSet)

import Syntax.Name
import Syntax.QName
import Util.TopoSort

data ModuleHeader = ModuleHeader
  { moduleName :: !ModuleName
  , moduleExposedNames :: ExposedNames
  , moduleImports :: [Import]
  } deriving (Eq, Show)

data ExposedNames
  = Exposed (HashSet Name) -- TODO allow qualified names
  | AllExposed
  deriving (Eq, Show)

noneExposed :: ExposedNames
noneExposed = Exposed mempty

data Import = Import
  { importModule :: !ModuleName
  , importAlias :: !ModuleName
  , importExposedNames :: !ExposedNames
  } deriving (Eq, Show)

moduleDependencyOrder
  :: Foldable t
  => t (ModuleHeader, contents)
  -> [SCC (ModuleHeader, contents)]
moduleDependencyOrder = topoSortWith (moduleName . fst) $ fmap importModule . moduleImports . fst
