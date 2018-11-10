{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Driver.Query where

import Protolude

import Data.Dependent.Map(DMap)
import qualified Data.Dependent.Map as DMap
import Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Rock
import Text.Parsix.Position

import Error
import qualified Frontend.Parse as Parse
import Processor.Result
import Syntax
import qualified Syntax.Core as Core
import qualified Syntax.Pre.Definition as Pre
import qualified Syntax.Pre.Scoped as Pre
import qualified Syntax.Pre.Unscoped as Unscoped

type ModuleDefinitions = HashMap QName (SourceLoc, Unscoped.TopLevelDefinition)
type ResolvedModule = HashMap QName [(QName, SourceLoc, Closed (Pre.Definition Pre.Expr))]
type TypeCheckedGroup = HashMap QName (SourceLoc, ClosedDefinition Core.Expr, Biclosed Core.Expr)

data Query a where
  -- Frontend
  Files :: Query [FilePath]
  File :: FilePath -> Query Text
  ParsedModule :: FilePath -> Query (ModuleHeader, [(SourceLoc, Unscoped.TopLevelDefinition)])
  ModuleHeaders :: Query (HashMap FilePath ModuleHeader)
  ResolvedModule :: ModuleName -> Query ResolvedModule
  TypeCheckedGroup :: QName -> Query TypeCheckedGroup

  Type :: QName -> Query (Biclosed Core.Expr)
  Definition :: QName -> Query (ClosedDefinition Core.Expr)

noError :: (Monoid w, Functor f) => f a -> f (a, w)
noError = fmap (, mempty)

rules :: [FilePath] -> GenRules (Writer [Error] Query) Query
rules inputFiles (Writer query) = case query of
  Files -> Input $ noError $ return inputFiles
  File file -> Input $ noError $ readFile file
  ParsedModule file -> Task $ do
    text <- fetch $ File file
    case Parse.parseText Parse.modul text file of
      Failure errs -> do
        let mh = ModuleHeader "Main" mempty mempty
        return ((mh, mempty), errs)
      Success a -> return (a, mempty)
  ModuleHeaders -> Task $ noError $ do
    fileNames <- fetch Files
    result <- for fileNames $ \file ->
      (,) file <$> fetchModuleHeader file
    return $ HashMap.fromList result
  ResolvedModule moduleName -> undefined
  TypeCheckedGroup name -> undefined
  Type name -> undefined
  Definition name -> undefined

-- Derived queries
fetchModuleHeader :: FilePath -> Task Query ModuleHeader
fetchModuleHeader file = fst <$> fetch (ParsedModule file)
