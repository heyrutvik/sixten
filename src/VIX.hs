{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module VIX (Env(..), VIX, hoistIO) where

import Protolude hiding (TypeError)

import Control.Lens
import Control.Monad.Morph
import Rock

import Driver.Query
import Effect.Fresh
import Effect.Log
import Effect.Report

data Env = Env
  { _logEnv :: !LogEnv
  , _reportEnv :: !ReportEnv
  , _freshEnv :: !FreshEnv
  }

type VIX = ReaderT Env (Task Query)

makeLenses ''Env

instance HasLogEnv Env where logEnv = VIX.logEnv
instance HasReportEnv Env where reportEnv = VIX.reportEnv
instance HasFreshEnv Env where freshEnv = VIX.freshEnv

hoistIO :: ReaderT Env IO a -> VIX a
hoistIO = hoist liftIO
