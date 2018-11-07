{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Driver.Query where

import Protolude

import Data.HashMap.Lazy(HashMap)
import Rock
import Text.Parsix.Position

import Syntax
import qualified Syntax.Core as Core
import qualified Syntax.Pre.Definition as Pre
import qualified Syntax.Pre.Scoped as Pre
import qualified Syntax.Pre.Unscoped as Unscoped

type ParsedModule = HashMap QName (SourceLoc, Unscoped.TopLevelDefinition)
type ResolvedModule = HashMap QName [(QName, SourceLoc, Closed (Pre.Definition Pre.Expr))]
type TypeCheckedGroup = HashMap QName (SourceLoc, ClosedDefinition Core.Expr, Biclosed Core.Expr)

data Query a where
  -- Frontend
  Files :: Query [FilePath]
  File :: FilePath -> Query Text
  ModuleHeader :: FilePath -> Query (ModuleHeader, Position)
  ModuleHeaders :: Query (HashMap FilePath ModuleHeader)
  ParsedModule :: ModuleName -> Query ParsedModule
  ResolvedModule :: ModuleName -> Query ResolvedModule
  TypeCheckedGroup :: QName -> Query TypeCheckedGroup

  Type :: QName -> Query (Biclosed Core.Expr)
  Definition :: QName -> Query (ClosedDefinition Core.Expr)

rules :: [FilePath] -> Rules Query
rules inputFiles query = case query of
  Files -> Input $ return inputFiles
  File file -> Input $ readFile file
  Driver.Query.ModuleHeader file -> undefined
  ModuleHeaders -> do
    fileNames <- fetch Files
    forM fileNames $ \file ->
      fetch $ File file

  ParsedModule -> undefined
  ResolvedModule -> undefined
  TypeCheckedGroup -> undefined

  Type -> undefined
  Definition -> undefined

-- Derived queries
fetchModuleHeader :: FilePath -> Task Query ModuleHeader
fetchModuleHeader = fst <$> fetch ParseModuleHeader
