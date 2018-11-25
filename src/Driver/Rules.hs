{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Driver.Rules where

import Protolude hiding (moduleName)

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet as HashSet
import Rock

import Backend.Target
import Driver.Query
import Effect
import qualified Elaboration.Monad as TypeCheck
import qualified Elaboration.TypeCheck.Definition as TypeCheck
import Error
import Frontend.DupCheck as DupCheck
import qualified Frontend.Parse as Parse
import qualified Frontend.ResolveNames as ResolveNames
import Syntax
import qualified Syntax.Pre.Unscoped as Unscoped
import Util
import VIX

noError :: (Monoid w, Functor f) => f a -> f (a, w)
noError = fmap (, mempty)

withReportEnv :: MonadIO m => (ReportEnv -> m a) -> m (a, [Error])
withReportEnv f = do
  errVar <- liftIO $ newMVar []
  a <- f ReportEnv
    { _currentLocation = Nothing
    , _reportAction = \err ->
      modifyMVar_ errVar $ \errs -> pure $ err:errs
    }
  errs <- liftIO $ readMVar errVar
  return (a, errs)

rules
  :: LogEnv
  -> [FilePath]
  -> Target
  -> GenRules (Writer [Error] Query) Query
rules logEnv_ inputFiles target (Writer query) = case query of
  Files -> Input $ noError $ return inputFiles

  File file -> Input $ noError $ readFile file

  Driver.Query.Target -> Input $ noError $ return target

  ParsedModule file -> Task $ do
    text <- fetch $ File file
    case Parse.parseText Parse.modul text file of
      Left err -> do
        let mh = ModuleHeader "Main" noneExposed mempty
        return ((mh, mempty), pure err)
      Right a -> return (a, mempty)

  ModuleHeaders -> Task $ noError $ do
    fileNames <- fetch Files
    result <- for fileNames $ \file ->
      (,) file <$> fetchModuleHeader file
    return $ HashMap.fromList result

  ModuleFiles -> Task $ noError $ do
    moduleHeaders <- fetch ModuleHeaders
    return
      $ HashMap.fromList
      [(moduleName mh, fp) | (fp, mh) <- HashMap.toList moduleHeaders]

  ModuleFile moduleName_ -> Task $ noError $
    HashMap.lookupDefault "Main" moduleName_ <$> fetch ModuleFiles

  DupCheckedModule moduleName_ -> Task $ do
    file <- fetch $ ModuleFile moduleName_
    (moduleHeader_, defs) <- fetch $ ParsedModule file
    return $ DupCheck.dupCheck (moduleName moduleHeader_) defs

  ModuleExports moduleName_ -> Task $ noError $ do
    file <- fetch $ ModuleFile moduleName_
    (moduleHeader_, _) <- fetch $ ParsedModule file
    defs <- fetch $ DupCheckedModule moduleName_

    let
      p = case moduleExposedNames moduleHeader_ of
        AllExposed -> const True
        Exposed names -> (`HashSet.member` toHashSet names)

      defNames = HashSet.filter (p . qnameName) $ HashSet.fromMap $ void defs
      conNames = HashSet.fromList
        [ QConstr n c
        | (n, (_, Unscoped.TopLevelDataDefinition _ _ cs)) <- HashMap.toList defs
        , c <- constrName <$> cs
        , p $ qnameName n
        ]
      methods = HashSet.fromList
        [ QName n m
        | (QName n _, (_, Unscoped.TopLevelClassDefinition _ _ ms)) <- HashMap.toList defs
        , m <- methodName <$> ms
        , p m
        ]
    return (defNames <> methods, conNames)

  ResolvedModule moduleName_ -> Task $ do
    defs <- fetch $ DupCheckedModule moduleName_
    file <- fetch $ ModuleFile moduleName_
    (moduleHeader_, _) <- fetch $ ParsedModule file
    (bindingGroups, errs) <- withReportEnv $ \reportEnv_ ->
      runVIX logEnv_ reportEnv_ $
        ResolveNames.resolveModule moduleHeader_ defs
    let
      bindingGroupMap =
        HashMap.fromList
          [(HashSet.fromList names, xs)
          | xs <- bindingGroups
          , let names = fst3 <$> xs
          ]
    return (bindingGroupMap, errs)

  TypeCheckedGroup names -> Task $
    case toList names of
      [] -> return mempty
      name:_ -> do
        let moduleName_ = qnameModule name
        bindingGroups <- fetch $ ResolvedModule moduleName_
        let
          bindingGroup
            = fromMaybe (panic "No such module")
            $ HashMap.lookup names bindingGroups
        (result, errs) <- withReportEnv $ \reportEnv_ ->
           runVIX logEnv_ reportEnv_
            $ TypeCheck.runElaborate moduleName_
            $ TypeCheck.checkAndGeneraliseTopLevelDefs
            $ toVector bindingGroup
        let resultMap = toHashMap $ (\(n, l, d, t) -> (n, (l, d, t))) <$> result
        return (resultMap, errs)

  BindingGroup name -> Task $ noError $ do
    bindingGroups <- fetch $ ResolvedModule $ qnameModule name
    case filter (HashSet.member name) $ HashMap.keys bindingGroups of
      [] -> return mempty
      result:_ -> return result
