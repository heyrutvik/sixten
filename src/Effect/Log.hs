{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Effect.Log where

import Protolude hiding (log)

import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Identity
import Control.Monad.Writer

import qualified Pretty
import Pretty(Pretty, pretty)

class Monad m => MonadLog m where
  getVerbosity :: m Int
  log :: Text -> m ()
  indent :: m a -> m a

  default getVerbosity
    :: (MonadTrans t, MonadLog m1, m ~ t m1)
    => m Int
  getVerbosity = lift getVerbosity

  default log
    :: (MonadTrans t, MonadLog m1, m ~ t m1)
    => Text -> m ()
  log = lift . log

logVerbose :: MonadLog m => Int -> Text -> m ()
logVerbose i = whenVerbose i . log

whenVerbose :: MonadLog m => Int -> m () -> m ()
whenVerbose i m = do
  v <- getVerbosity
  when (v >= i) m

logPretty :: (MonadLog m, Pretty a) => Int -> Text -> a -> m ()
logPretty v s x = whenVerbose v $
  log $ "--" <> s <> ": " <> Pretty.showWide (pretty x)

logShow :: (MonadLog m, Show a) => Int -> Text -> a -> m ()
logShow v s x = whenVerbose v $
  log $ "--" <> s <> ": " <> show x

data LogEnv = LogEnv
  { _verbosity :: !Int
  , _logAction :: !(Text -> IO ())
  }

makeLenses ''LogEnv

class HasLogEnv env where
  logEnv :: Lens' env LogEnv

instance (MonadIO m, HasLogEnv env) => MonadLog (ReaderT env m) where
  getVerbosity = view $ logEnv.verbosity
  log t = do
    f <- view $ logEnv.logAction
    liftIO $ f t
  indent = local
    $ over (logEnv . logAction)
    $ \f x -> f ("| " <> x)

instance Semigroup LogEnv where
  LogEnv v1 log1 <> LogEnv v2 log2 = LogEnv (max v1 v2) (\t -> log1 t *> log2 t)

instance Monoid LogEnv where
  mempty = LogEnv 0 $ \_ -> pure ()

-------------------------------------------------------------------------------
-- mtl instances
-------------------------------------------------------------------------------
instance (Monoid w, MonadLog m) => MonadLog (WriterT w m) where
  indent (WriterT m) = WriterT $ indent m
instance MonadLog m => MonadLog (StateT s m) where
  indent (StateT s) = StateT $ indent <$> s
instance MonadLog m => MonadLog (IdentityT m) where
  indent (IdentityT m) = IdentityT $ indent m
instance MonadLog m => MonadLog (ExceptT e m) where
  indent (ExceptT m) = ExceptT $ indent m
