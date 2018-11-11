{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module MonadLog where

import Protolude

import Control.Lens
import Control.Lens.TH

class Monad m => MonadLog m where
  verbosity :: m Int
  log :: Text -> m ()
  indentLog :: m a -> m a

logVerbose :: MonadLog m => Int -> Text -> m ()
logVerbose i = whenVerbose i . MonadLog.log

whenVerbose :: MonadLog m => Int -> m () -> m ()
whenVerbose i m = do
  v <- verbosity
  when (v >= i) m

data LogEnv m = LogEnv
  { _envVerbosity :: !Int
  , _envLog :: !(Text -> m ())
  }

makeFieldsNoPrefix ''LogEnv

class HasLogEnv env m | env -> m where
  logEnv :: Lens' env (LogEnv m)

instance HasLogEnv env m => MonadLog (ReaderT env m) where
  verbosity = view $ logEnv.envVerbosity
  log t = do
    f <- view $ logEnv.envLog
    f t
  indentLog = local
    $ over (logEnv . envLog)
    $ \f x -> f ("| " <> x)
