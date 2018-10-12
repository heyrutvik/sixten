{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, MultiParamTypeClasses, OverloadedStrings, UndecidableInstances #-}
module MonadContext where

import Protolude

import Control.Monad.Except
import Control.Monad.Trans.Identity
import Control.Monad.Writer
import Data.Vector(Vector)

import MonadFresh
import MonadLog
import Syntax
import Syntax.Telescope
import TypedFreeVar
import Util
import Util.Tsil

class Monad m => MonadContext v m | m -> v where
  inUpdatedContext :: (Tsil v -> Tsil v) -> m a -> m a
  localVars :: m (Tsil v)

  default localVars
    :: (MonadTrans t, MonadContext v m1, m ~ t m1)
    => m (Tsil v)
  localVars = lift localVars

withVar :: MonadContext v m => v -> m a -> m a
withVar v = inUpdatedContext $ \vs -> Snoc vs v

withVars :: (MonadContext v m, Foldable t) => t v -> m a -> m a
withVars vs m = foldr withVar m vs

inContext
  :: (MonadContext (FreeVar d e) m, MonadFresh m, MonadLog m)
  => NameHint
  -> d
  -> e (FreeVar d e)
  -> (FreeVar d e -> m a)
  -> m a
inContext h p t k = do
  v <- freeVar h p t
  logVerbose 20 $ "forall: " <> shower (varId v)
  withVar v $ k v

inTeleContext
  :: (MonadContext (FreeVar d e) m, MonadFresh m, MonadLog m)
  => Telescope d e v
  -> (Vector (FreeVar d e) -> m a)
  -> m a
inTeleContext tele k = do
  vs <- forTeleWithPrefixM tele $ \h p s vs ->
    freeVar h p $ instantiateTele pure vs s
  k vs

-------------------------------------------------------------------------------
-- mtl instances
-------------------------------------------------------------------------------
instance MonadContext v m => MonadContext v (ReaderT r m) where
  inUpdatedContext f (ReaderT m) = ReaderT $ inUpdatedContext f . m
instance (Monoid w, MonadContext v m) => MonadContext v (WriterT w m) where
  inUpdatedContext f (WriterT m) = WriterT $ inUpdatedContext f m
instance MonadContext v m => MonadContext v (StateT s m) where
  inUpdatedContext f (StateT m) = StateT $ inUpdatedContext f . m
instance MonadContext v m => MonadContext v (IdentityT m) where
  inUpdatedContext f (IdentityT m) = IdentityT $ inUpdatedContext f m
instance MonadContext v m => MonadContext v (ExceptT e m) where
  inUpdatedContext f (ExceptT m) = ExceptT $ inUpdatedContext f m
