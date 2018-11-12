{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Effect.Report where

import Protolude hiding (TypeError)

import Control.Lens
import Control.Monad.Trans

import Error
import Pretty

class Monad m => MonadReport m where
  report :: Error -> m ()
  located :: SourceLoc -> m a -> m a
  getCurrentLocation :: m (Maybe SourceLoc)

  default report
    :: (MonadTrans t, MonadReport m1, m ~ t m1)
    => Error -> m ()
  report = lift . report

  default getCurrentLocation
    :: (MonadTrans t, MonadReport m1, m ~ t m1)
    => m (Maybe SourceLoc)
  getCurrentLocation = lift getCurrentLocation

reportLocated :: MonadReport m => Doc -> m ()
reportLocated x = do
  loc <- getCurrentLocation
  report $ TypeError x loc mempty

data ReportEnv = ReportEnv
  { _currentLocation :: !(Maybe SourceLoc)
  , _reportAction :: !(Error -> IO ())
  }

emptyReportEnv :: (Error -> IO ()) -> ReportEnv
emptyReportEnv = ReportEnv Nothing

makeLenses ''ReportEnv

class HasReportEnv env where
  reportEnv :: Lens' env ReportEnv

instance (MonadIO m, HasReportEnv env) => MonadReport (ReaderT env m) where
  report e = do
    f <- view $ reportEnv.reportAction
    liftIO $ f e
  located = local . set (reportEnv.currentLocation) . Just
  getCurrentLocation = view $ reportEnv.currentLocation
