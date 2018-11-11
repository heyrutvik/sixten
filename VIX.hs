{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module VIX where

import Protolude hiding (TypeError)

import Control.Lens
import Rock

import Driver.Query
import Error
import MonadFresh
import Pretty
import Syntax
import TypedFreeVar
import Util

data ReportEnv = ReportEnv
  { _sourceLoc :: !(Maybe SourceLoc)
  , _reportError :: !(Error -> IO ())
  }

makeFieldsNoPrefix ''ReportEnv

data Env = Env
  { _logEnv :: !LogEnv
  , _reportEnv :: !ReportEnv
  , _freshEnv :: !FreshEnv
  }

makeFieldsNoPrefix ''Env

type VIX = ReaderT Env (Task Query)

-------------------------------------------------------------------------------
-- Logging

instance MonadIO m => MonadLog (ReaderT Env m) where
  log t = do
    f <- view $ logEnv.logText
    liftIO $ f t

  verbosity = view $ logEnv . VIX.verbosity

-------------------------------------------------------------------------------
-- Error reporting

type MonadReport env m = (MonadIO m, MonadReader env m, HasReportEnv env ReportEnv)

report :: MonadReport env m => Error -> m ()
report e = do
  f <- view $ reportEnv.reportError
  liftIO $ f e

reportLocated :: MonadReport env m => Doc -> m ()
reportLocated x = do
  loc <- currentLocation
  report $ TypeError x loc mempty

located :: MonadReport env m => SourceLoc -> m a -> m a
located = local . set (reportEnv.sourceLoc) . Just

currentLocation :: MonadReport env m => m (Maybe SourceLoc)
currentLocation = view $ reportEnv.sourceLoc

-------------------------------------------------------------------------------
-- Instances

instance HasFreshEnv Env where
  freshEnv = VIX.freshEnv

-------------------------------------------------------------------------------
-- Variables

-- | Like freeVar, but with logging
forall
  :: (MonadFresh m, MonadLog env m)
  => NameHint
  -> d
  -> e (FreeVar d e)
  -> m (FreeVar d e)
forall h p t = do
  v <- freeVar h p t
  logVerbose 20 $ "forall: " <> shower (varId v)
  return v

logFreeVar
  :: (Functor e, Functor f, Foldable f, Pretty (f Doc), Pretty (e Doc), MonadLog env m)
  => Int
  -> Text
  -> f (FreeVar d e)
  -> m ()
logFreeVar v s x = whenVerbose v $ do
  let r = showFreeVar x
  VIX.log $ "--" <> s <> ": " <> showWide r
