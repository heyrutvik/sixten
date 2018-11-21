{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Driver.Rules where

import Protolude hiding (moduleName)

import qualified Data.HashMap.Lazy as HashMap
import Rock

import Backend.Target
import Driver.Query
import Error
import qualified Frontend.Parse as Parse
import Syntax

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
        let mh = ModuleHeader "Main" mempty mempty
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

  ModuleExports moduleName_ -> Task $ noError $ do
    file <- moduleFile moduleName_
    parsed <- fetch $ ParsedModule fp
