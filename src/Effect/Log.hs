{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Effect.Log where

import Protolude

import Control.Lens

class Monad m => MonadLog m where
  getVerbosity :: m Int
  log :: Text -> m ()
  indent :: m a -> m a

logVerbose :: MonadLog m => Int -> Text -> m ()
logVerbose i = whenVerbose i . Effect.Log.log

whenVerbose :: MonadLog m => Int -> m () -> m ()
whenVerbose i m = do
  v <- getVerbosity
  when (v >= i) m

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
