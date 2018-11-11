{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Elaboration.Monad where

import Protolude

import Control.Lens.TH
import Control.Monad.Except
import Control.Monad.Fail
import qualified Data.Vector as Vector
import Rock

import qualified Builtin.Names as Builtin
import Driver.Query
import Elaboration.MetaVar
import MonadContext
import MonadFresh
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
  { _localVariables :: Tsil FreeV
  , _elabTouchables :: !(MetaVar -> Bool)
  , _vixEnv :: !VIX.Env
  }

makeFieldsNoPrefix ''ElabEnv

instance HasLogEnv ElabEnv LogEnv where
  logEnv = vixEnv.logEnv

instance HasReportEnv ElabEnv ReportEnv where
  reportEnv = vixEnv.reportEnv

instance HasFreshEnv ElabEnv FreshEnv where
  freshEnv = vixEnv.freshEnv

type Elaborate = ReaderT ElabEnv (Task Query)

runElaborate :: Elaborate a -> VIX a
runElaborate = withReader $ \env -> ElabEnv
  { _localVariables = mempty
  , _elabTouchables = const True
  , _vixEnv = env
  }

instance MonadContext FreeV Elaborate where
  localVars = view localVariables

  inUpdatedContext f (Elaborate m) = do
    vs <- view localVariables
    let vs' = f vs
    logShow 30 "local variable scope" (varId <$> toList vs')
    indentLog $
      local
        (\env -> env { localVariables = vs' })
        m

exists
  :: NameHint
  -> Plicitness
  -> CoreM
  -> Elaborate CoreM
exists hint d typ = do
  locals <- toVector . Tsil.filter (isNothing . varValue) <$> localVars
  let typ' = Core.pis locals typ
  logMeta 30 "exists typ" typ
  let typ'' = close (panic "exists not closed") typ'
  loc <- currentLocation
  v <- explicitExists hint d typ'' (Vector.length locals) loc
  return $ Core.Meta v $ (\fv -> (varData fv, pure fv)) <$> locals

existsType
  :: NameHint
  -> Elaborate CoreM
existsType n = exists n Explicit Builtin.Type

getTouchable :: Elaborate (MetaVar -> Bool)
getTouchable = Elaborate $ asks elabTouchables

untouchable :: Elaborate a -> Elaborate a
untouchable (Elaborate i) = do
  v <- fresh
  Elaborate $ local (\s -> s { elabTouchables = \m -> elabTouchables s m && metaId m > v }) i
