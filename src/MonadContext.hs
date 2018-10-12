{-# LANGUAGE DefaultSignatures, FlexibleInstances, FunctionalDependencies, GADTs, MultiParamTypeClasses, UndecidableInstances #-}
module MonadContext where

import Protolude

import Control.Monad.Except
import Control.Monad.Trans.Identity
import Control.Monad.Writer

import MonadFresh
import MonadLog
import Syntax
import Util.Tsil
import TypedFreeVar

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
