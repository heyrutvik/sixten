{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module MonadContext where

import Protolude

import Control.Lens
import Control.Lens.TH

import Control.Monad.Except
import Control.Monad.ListT
import Control.Monad.Trans.Identity
import Control.Monad.Writer

import Util.Tsil

newtype ContextEnv v = ContextEnv
  { _localVars :: Tsil v
  }

makeFieldsNoPrefix ''ContextEnv

type MonadContext v env m = (MonadReader env m, HasLocalVars env (Tsil v))

inUpdatedContext :: MonadContext v env m => (Tsil v -> Tsil v) -> m a -> m a
inUpdatedContext = local . over localVars

withVar :: MonadContext v env m => v -> m a -> m a
withVar v = inUpdatedContext $ \vs -> Snoc vs v

withVars :: (MonadContext v env m, Foldable t) => t v -> m a -> m a
withVars vs m = foldr withVar m vs
