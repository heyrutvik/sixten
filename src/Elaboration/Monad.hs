{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Elaboration.Monad where

import Protolude

import Control.Lens
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Reader
import qualified Data.Vector as Vector
import Rock

import qualified Builtin.Names as Builtin
import Driver.Query
import Effect
import Elaboration.MetaVar
import Syntax
import qualified Syntax.Core as Core
import qualified Syntax.Pre.Scoped as Pre
import TypedFreeVar
import Util
import Util.Tsil(Tsil)
import qualified Util.Tsil as Tsil
import VIX

type PreM = Pre.Expr FreeV
type CoreM = Core.Expr MetaVar FreeV

type Polytype = CoreM
type Rhotype = CoreM -- No top-level foralls

newtype InstUntil = InstUntil Plicitness
  deriving (Eq, Ord, Show)

shouldInst :: Plicitness -> InstUntil -> Bool
shouldInst Explicit _ = False
shouldInst _ (InstUntil Explicit) = True
shouldInst p (InstUntil p') | p == p' = False
shouldInst _ _ = True

data ElabEnv = ElabEnv
  { _contextEnv :: !(ContextEnv FreeV)
  , _elabTouchables :: !(MetaVar -> Bool)
  , _vixEnv :: !VIX.Env
  }

makeLenses ''ElabEnv

instance HasLogEnv ElabEnv where
  logEnv = vixEnv.logEnv

instance HasReportEnv ElabEnv where
  reportEnv = vixEnv.reportEnv

instance HasFreshEnv ElabEnv where
  freshEnv = vixEnv.freshEnv

instance HasContextEnv FreeV ElabEnv where
  contextEnv = Elaboration.Monad.contextEnv

type Elaborate = ReaderT ElabEnv (Task Query)

runElaborate :: Elaborate a -> VIX a
runElaborate = withReaderT $ \env -> ElabEnv
  { _contextEnv = emptyContextEnv
  , _elabTouchables = const True
  , _vixEnv = env
  }

exists
  :: NameHint
  -> Plicitness
  -> CoreM
  -> Elaborate CoreM
exists hint d typ = do
  locals <- toVector . Tsil.filter (isNothing . varValue) <$> getLocalVars
  let typ' = Core.pis locals typ
  logMeta 30 "exists typ" typ
  let typ'' = close (panic "exists not closed") typ'
  loc <- getCurrentLocation
  v <- explicitExists hint d typ'' (Vector.length locals) loc
  return $ Core.Meta v $ (\fv -> (varData fv, pure fv)) <$> locals

existsType
  :: NameHint
  -> Elaborate CoreM
existsType n = exists n Explicit Builtin.Type

getTouchable :: Elaborate (MetaVar -> Bool)
getTouchable = view elabTouchables

untouchable :: Elaborate a -> Elaborate a
untouchable i = do
  v <- fresh
  local (over elabTouchables $ \t m -> t m && metaId m > v) i
