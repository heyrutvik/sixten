{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Driver.Query (module Rock, module Driver.Query) where

import Protolude hiding (TypeRep)

import Data.HashMap.Lazy(HashMap)
import Data.HashSet(HashSet)
import qualified Data.HashSet as HashSet
import Rock
import Util.MultiHashMap(MultiHashMap)
import qualified Util.MultiHashMap as MultiHashMap

import Backend.Target as Target
import Error
import Syntax
import qualified Syntax.Core as Core
import qualified Syntax.Pre.Definition as Pre
import qualified Syntax.Pre.Scoped as Pre
import qualified Syntax.Pre.Unscoped as Unscoped
import qualified Syntax.Sized.Lifted as Lifted
import TypeRep

type ModuleDefinitions = HashMap QName (SourceLoc, Unscoped.TopLevelDefinition)
type ResolvedModule = HashMap (HashSet QName) [(QName, SourceLoc, Closed (Pre.Definition Pre.Expr))]
type ElaboratedGroup = HashMap QName (SourceLoc, ClosedDefinition Core.Expr, Biclosed Core.Expr)

data Query a where
  Files :: Query [FilePath]
  File :: FilePath -> Query Text
  Target :: Query Target
  ParsedModule :: FilePath -> Query (ModuleHeader, [(SourceLoc, Unscoped.TopLevelDefinition)])
  ModuleHeaders :: Query (HashMap FilePath ModuleHeader)
  ModuleFiles :: Query (HashMap ModuleName FilePath)
  ModuleFile :: ModuleName -> Query FilePath
  DupCheckedModule :: ModuleName -> Query (HashMap QName (SourceLoc, Unscoped.TopLevelDefinition))
  ModuleExports :: ModuleName -> Query (HashSet QName, HashSet QConstr)
  ResolvedModule :: ModuleName -> Query ResolvedModule
  ElaboratedGroup :: HashSet QName -> Query ElaboratedGroup
  BindingGroup :: QName -> Query (HashSet QName)

  Type :: QName -> Query (Biclosed Core.Expr)
  Definition :: QName -> Query (ClosedDefinition Core.Expr)
  QConstructor :: QConstr -> Query (Biclosed Core.Expr)
  -- TODO should perhaps be derived?
  ClassMethods :: QName -> Query (Maybe [(Name, SourceLoc)])
  Instances :: ModuleName -> Query (MultiHashMap QName QName)

  ConstrIndex :: QConstr -> Query (Maybe Integer)

  ConvertedSignature :: QName -> Query (Maybe Lifted.FunSignature)
  Signature :: QName -> Query (Maybe (Signature ReturnIndirect))

-- Derived queries
fetchModuleHeader :: MonadFetch Query m => FilePath -> m ModuleHeader
fetchModuleHeader file = fst <$> fetch (ParsedModule file)

fetchDefinition :: MonadFetch Query m => QName -> m (Definition (Core.Expr meta) v)
fetchDefinition name = openDefinition <$> fetch (Definition name)

fetchType :: MonadFetch Query m => QName -> m (Core.Type meta v)
fetchType name = biopen <$> fetch (Type name)

fetchInstances :: MonadFetch Query m => QName -> ModuleName -> m [(QName, Core.Type meta a)]
fetchInstances className moduleName_ = do
  classInstances <- fetch $ Instances moduleName_
  let instanceNames = MultiHashMap.lookup className classInstances
  forM (HashSet.toList instanceNames) $ \name -> do
    typ <- fetchType name
    return (name, typ)

fetchQConstructor :: MonadFetch Query m => QConstr -> m (Core.Type meta v)
fetchQConstructor qc = biopen <$> fetch (QConstructor qc)

fetchIntRep :: MonadFetch Query m => m TypeRep
fetchIntRep = TypeRep.intRep <$> fetch Driver.Query.Target

fetchTypeRep :: MonadFetch Query m => m TypeRep
fetchTypeRep = TypeRep.typeRep <$> fetch Driver.Query.Target

fetchPtrRep :: MonadFetch Query m => m TypeRep
fetchPtrRep = TypeRep.ptrRep <$> fetch Driver.Query.Target

fetchPiRep :: MonadFetch Query m => m TypeRep
fetchPiRep = TypeRep.piRep <$> fetch Driver.Query.Target
