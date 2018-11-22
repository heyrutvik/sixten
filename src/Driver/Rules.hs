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
import Error
import Frontend.DupCheck as DupCheck
import qualified Frontend.Parse as Parse
import Syntax
import qualified Syntax.Pre.Unscoped as Unscoped
import Util

noError :: (Monoid w, Functor f) => f a -> f (a, w)
noError = fmap (, mempty)

rules :: [FilePath] -> Target -> GenRules (Writer [Error] Query) Query
rules inputFiles target (Writer query) = case query of
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
