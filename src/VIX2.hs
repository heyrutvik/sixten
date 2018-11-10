{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module VIX2 where

import Protolude

import Control.Lens

import Driver.Query
import Error hiding (MonadReport)
import Rock

data LogEnv = LogEnv
  { _verbosity :: !Int
  , _logText :: !(Text -> IO ())
  }

makeFieldsNoPrefix ''LogEnv

data Env = Env
  { _logEnv :: !LogEnv
  , _reportError :: !(Error -> IO ())
  , _sourceLoc :: !(Maybe SourceLoc)
  }

makeFieldsNoPrefix ''Env

type VIX a = ReaderT Env (Task Query) a

-------------------------------------------------------------------------------
-- Logging

type MonadLog env m = (MonadIO m, MonadReader env m, HasLogEnv env LogEnv)

log :: MonadLog env m => Text -> m ()
log t = do
  f <- view $ logEnv.logText
  liftIO $ f t

whenVerbose :: MonadLog env m => Int -> m () -> m ()
whenVerbose i m = do
  v <- view $ logEnv.verbosity
  when (v >= i) m

logVerbose :: MonadLog env m => Int -> Text -> m ()
logVerbose i = whenVerbose i . VIX2.log

indentLog :: MonadLog env m => m a -> m a
indentLog
  = local
  $ over (logEnv . VIX2.logText)
  $ \f x -> f ("| " <> x)

-------------------------------------------------------------------------------
-- Error reporting

type MonadReport env m = (MonadIO m, MonadReader env m, HasReportError env (Error -> IO ()))

report :: MonadReport env m => Error -> m ()
report e = do
  f <- view reportError
  liftIO $ f e

located :: (MonadReader env m, HasSourceLoc env (Maybe SourceLoc)) => SourceLoc -> m a -> m a
located = local . set sourceLoc . Just

currentLocation :: (MonadReader env m, HasSourceLoc env (Maybe SourceLoc)) => m (Maybe SourceLoc)
currentLocation = view sourceLoc
