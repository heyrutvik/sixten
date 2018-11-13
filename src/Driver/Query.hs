{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Driver.Query (module Rock, module Driver.Query) where

import Protolude

import Data.HashMap.Lazy(HashMap)
import Data.HashSet(HashSet)
import Rock

import Backend.Target
import Error
import Syntax
import qualified Syntax.Core as Core
import qualified Syntax.Pre.Definition as Pre
import qualified Syntax.Pre.Scoped as Pre
import qualified Syntax.Pre.Unscoped as Unscoped

type ModuleDefinitions = HashMap QName (SourceLoc, Unscoped.TopLevelDefinition)
type ResolvedModule = HashMap QName [(QName, SourceLoc, Closed (Pre.Definition Pre.Expr))]
type TypeCheckedGroup = HashMap QName (SourceLoc, ClosedDefinition Core.Expr, Biclosed Core.Expr)

data Query a where
  Files :: Query [FilePath]
  File :: FilePath -> Query Text
  Target :: Query Target
  ParsedModule :: FilePath -> Query (ModuleHeader, [(SourceLoc, Unscoped.TopLevelDefinition)])
  ModuleHeaders :: Query (HashMap FilePath ModuleHeader)
  ModuleExports :: ModuleName -> Query (HashSet QName, HashSet QConstr)
  ResolvedModule :: ModuleName -> Query ResolvedModule
  TypeCheckedGroup :: QName -> Query TypeCheckedGroup

  Type :: QName -> Query (Biclosed Core.Expr)
  Definition :: QName -> Query (ClosedDefinition Core.Expr)
  QConstructor :: QConstr -> Query (Biclosed Core.Expr)
  Instances :: QName -> ModuleName -> Query [(QName, Biclosed Core.Expr)]

-- Derived queries
fetchModuleHeader :: MonadFetch Query m => FilePath -> m ModuleHeader
fetchModuleHeader file = fst <$> fetch (ParsedModule file)

fetchDefinition :: MonadFetch Query m => QName -> m (Definition (Core.Expr meta) v)
fetchDefinition name = openDefinition <$> fetch (Definition name)

fetchType :: MonadFetch Query m => QName -> m (Core.Type meta v)
fetchType name = biopen <$> fetch (Type name)

fetchQConstructor :: MonadFetch Query m => QConstr -> m (Core.Type meta v)
fetchQConstructor qc = biopen <$> fetch (QConstructor qc)

fetchInstances :: MonadFetch Query m => QName -> m [(QName, Core.Expr meta v)]
fetchInstances name = fmap (fmap biopen) <$> fetch (Instances name)
