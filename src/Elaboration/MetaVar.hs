{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Elaboration.MetaVar where

import Protolude
import Prelude(showsPrec, showString, showChar, showParen)

import Data.Bitraversable
import Data.IORef
import Data.String
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Vector(Vector)
import qualified Data.Vector as Vector

import Error
import MonadContext
import MonadFresh
import MonadLog
import Syntax
import Syntax.Core
import TypedFreeVar
import Util
import VIX

type MetaRef = IORef (Maybe (Closed (Expr MetaVar)))

type FreeV = FreeVar Plicitness (Expr MetaVar)

data MetaVar = MetaVar
  { metaId :: !Int
  , metaType :: Closed (Expr MetaVar)
  , metaArity :: !Int
  , metaHint :: !NameHint
  , metaPlicitness :: !Plicitness
  , metaSourceLoc :: !(Maybe SourceLoc)
  , metaRef :: !MetaRef
  }

instance Eq MetaVar where
  (==) = (==) `on` metaId

instance Ord MetaVar where
  compare = compare `on` metaId

instance Hashable MetaVar where
  hashWithSalt s = hashWithSalt s . metaId

instance Show MetaVar where
  showsPrec d (MetaVar i (Closed t) a h p loc _) = showParen (d > 10) $
    showString "MetaVar" . showChar ' ' .
    showsPrec 11 i . showChar ' ' .
    showsPrec 11 (t :: Expr MetaVar Void) . showChar ' ' .
    showsPrec 11 a . showChar ' ' .
    showsPrec 11 h . showChar ' ' .
    showsPrec 11 p . showChar ' ' .
    showsPrec 11 (pretty <$> loc) . showChar ' ' .
    showString "<Ref>"

explicitExists
  :: (MonadVIX m, MonadLog m, MonadIO m)
  => NameHint
  -> Plicitness
  -> Closed (Expr MetaVar)
  -> Int
  -> Maybe SourceLoc
  -> m MetaVar
explicitExists hint p typ a loc = do
  i <- fresh
  ref <- liftIO $ newIORef Nothing
  logVerbose 20 $ "exists: " <> shower i
  logMeta 20 "exists typ: " (open typ :: Expr MetaVar Doc)
  return $ MetaVar i typ a hint p loc ref

solution
  :: MonadIO m
  => MetaVar
  -> m (Maybe (Closed (Expr MetaVar)))
solution = liftIO . readIORef . metaRef

solve
  :: MonadIO m
  => MetaVar
  -> Closed (Expr MetaVar)
  -> m ()
solve m x = liftIO $ writeIORef (metaRef m) $ Just x

isSolved :: MonadIO m => MetaVar -> m Bool
isSolved = fmap isJust . solution

isUnsolved :: MonadIO m => MetaVar -> m Bool
isUnsolved = fmap isNothing . solution

data WithVar a = WithVar !MetaVar a

instance Eq (WithVar a) where
  WithVar v1 _ == WithVar v2 _ = v1 == v2

instance Pretty a => Pretty (WithVar a) where
  prettyM (WithVar _ x) = prettyM x

prettyMetaVar
  :: (MonadIO m, MonadVIX m)
  => MetaVar
  -> m Doc
prettyMetaVar x = do
  let name = "?" <> fromNameHint "" fromName (metaHint x) <> shower (metaId x) <> "[" <> shower (metaArity x) <> "]"
  msol <- solution x
  case msol of
    Nothing -> return name
    Just sol -> do
      v <- liftVIX $ gets vixVerbosity
      sol' <- prettyMeta (open sol :: Expr MetaVar Doc)
      if v <= 30 then
        return $ PP.parens $ sol'
      else
        return $ PP.parens $ name PP.<+> "=" PP.<+> sol'

prettyMeta
  :: (Pretty v, MonadIO m, MonadVIX m)
  => Expr MetaVar v
  -> m Doc
prettyMeta e = do
  e' <- bitraverse (\m -> WithVar m <$> prettyMetaVar m) (pure . pretty) e
  return $ pretty e'

prettyDefMeta
  :: (Pretty v, MonadIO m, MonadVIX m)
  => Definition (Expr MetaVar) v
  -> m Doc
prettyDefMeta e = do
  e' <- bitraverseDefinition (\m -> WithVar m <$> prettyMetaVar m) (pure . pretty) e
  return $ pretty e'

logMeta
  :: (MonadLog m, Pretty v, MonadIO m, MonadVIX m)
  => Int
  -> String
  -> Expr MetaVar v
  -> m ()
logMeta v s e = whenVerbose v $ do
  i <- liftVIX $ gets vixIndent
  d <- prettyMeta e
  MonadLog.log $ mconcat (replicate i "| ") <> "--" <> fromString s <> ": " <> showWide d

logDefMeta
  :: (MonadIO m, Pretty v, MonadLog m, MonadVIX m)
  => Int
  -> String
  -> Definition (Expr MetaVar) v
  -> m ()
logDefMeta v s e = whenVerbose v $ do
  i <- liftVIX $ gets vixIndent
  d <- prettyDefMeta e
  MonadLog.log $ mconcat (replicate i "| ") <> "--" <> fromString s <> ": " <> showWide d

type FreeBindVar meta = FreeVar Plicitness (Expr meta)

instantiatedMetaType
  :: (MonadContext (FreeBindVar MetaVar) m, MonadError Error m, MonadVIX m, MonadIO m, MonadLog m)
  => MetaVar
  -> m (Vector FreeV, Expr MetaVar (FreeBindVar MetaVar))
instantiatedMetaType m = instantiatedMetaType' (metaArity m) m

instantiatedMetaType'
  :: (MonadContext (FreeBindVar MetaVar) m, MonadError Error m, MonadVIX m, MonadIO m, MonadLog m)
  => Int
  -> MetaVar
  -> m (Vector FreeV, Expr MetaVar (FreeBindVar MetaVar))
instantiatedMetaType' arity m = go mempty arity (open $ metaType m)
  where
    go vs 0 t = return (toVector $ reverse vs, t)
    go vs n (Pi h p t s) =
      inContext h p t $ \v ->
        go (v:vs) (n - 1) (instantiate1 (pure v) s)
    go _ _ _ = internalError "instantiatedMetaType'"

type MonadBindMetas meta' m = (MonadFresh m, MonadContext (FreeBindVar meta') m, MonadVIX m, MonadLog m, MonadIO m)

-- TODO move?
bindDefMetas
  :: MonadBindMetas meta' m
  => (meta -> Vector (Plicitness, Expr meta (FreeBindVar meta')) -> m (Expr meta' (FreeBindVar meta')))
  -> Definition (Expr meta) (FreeBindVar meta')
  -> m (Definition (Expr meta') (FreeBindVar meta'))
bindDefMetas f def = case def of
  ConstantDefinition a e -> ConstantDefinition a <$> bindMetas f e
  DataDefinition d rep -> DataDefinition <$> bindDataDefMetas f d <*> bindMetas f rep

bindDefMetas'
  :: MonadBindMetas meta' m
  => (meta -> Vector (Plicitness, Expr meta' (FreeBindVar meta')) -> m (Expr meta' (FreeBindVar meta')))
  -> Definition (Expr meta) (FreeBindVar meta')
  -> m (Definition (Expr meta') (FreeBindVar meta'))
bindDefMetas' f = bindDefMetas $ \m es -> do
  es' <- traverse (traverse $ bindMetas' f) es
  f m es'

bindDataDefMetas
  :: MonadBindMetas meta' m
  => (meta -> Vector (Plicitness, Expr meta (FreeBindVar meta')) -> m (Expr meta' (FreeBindVar meta')))
  -> DataDef (Expr meta) (FreeBindVar meta')
  -> m (DataDef (Expr meta') (FreeBindVar meta'))
bindDataDefMetas f (DataDef ps cs) = do
  vs <- forTeleWithPrefixM ps $ \h p s vs -> do
    let t = instantiateTele pure vs s
    t' <- bindMetas f t
    forall h p t'

  withVars vs $ do

    cs' <- forM cs $ \(ConstrDef c s) -> do
      e <- bindMetas f $ instantiateTele pure vs s
      return $ ConstrDef c e

    return $ dataDef vs cs'

bindMetas
  :: MonadBindMetas meta' m
  => (meta -> Vector (Plicitness, Expr meta (FreeBindVar meta')) -> m (Expr meta' (FreeBindVar meta')))
  -> Expr meta (FreeBindVar meta')
  -> m (Expr meta' (FreeBindVar meta'))
bindMetas f expr = case expr of
  Var v -> return $ Var v
  Meta m es -> f m es
  Global g -> return $ Global g
  Con c -> return $ Con c
  Lit l -> return $ Lit l
  Pi h p t s -> do
    t' <- bindMetas f t
    v <- forall h p t'
    let e = instantiate1 (pure v) s
    e' <- withVar v $ bindMetas f e
    return $ pi_ v e'
  Lam h p t s -> do
    t' <- bindMetas f t
    v <- forall h p t'
    let e = instantiate1 (pure v) s
    e' <- withVar v $ bindMetas f e
    return $ lam v e'
  App e1 p e2 -> App <$> bindMetas f e1 <*> pure p <*> bindMetas f e2
  Let ds scope -> do
    vs <- forMLet ds $ \h _ _ t -> do
      t' <- bindMetas f t
      forall h Explicit t'
    withVars vs $ do
      es <- forMLet ds $ \_ _ s _ -> do
        let e = instantiateLet pure vs s
        bindMetas f e
      let e = instantiateLet pure vs scope
      e' <- bindMetas f e
      return $ let_ (Vector.zip3 vs (letSourceLocs ds) es) e'
  Case e brs t -> Case <$> bindMetas f e <*> bindBranchMetas f brs <*> bindMetas f t
  ExternCode e t -> ExternCode <$> mapM (bindMetas f) e <*> bindMetas f t
  SourceLoc loc e -> SourceLoc loc <$> bindMetas f e

bindMetas'
  :: MonadBindMetas meta' m
  => (meta -> Vector (Plicitness, Expr meta' (FreeBindVar meta')) -> m (Expr meta' (FreeBindVar meta')))
  -> Expr meta (FreeBindVar meta')
  -> m (Expr meta' (FreeBindVar meta'))
bindMetas' f = bindMetas $ \m es -> do
  es' <- traverse (traverse $ bindMetas' f) es
  f m es'

bindBranchMetas
  :: MonadBindMetas meta' m
  => (meta -> Vector (Plicitness, Expr meta (FreeBindVar meta')) -> m (Expr meta' (FreeBindVar meta')))
  -> Branches Plicitness (Expr meta) (FreeBindVar meta')
  -> m (Branches Plicitness (Expr meta') (FreeBindVar meta'))
bindBranchMetas f brs = case brs of
  ConBranches cbrs -> ConBranches <$> do
    forM cbrs $ \(ConBranch c tele scope) -> do
      vs <- forTeleWithPrefixM tele $ \h p s vs -> do
        let t = instantiateTele pure vs s
        -- TODO inefficient: make special-case forTeleWithPrefix + withVars
        t' <- withVars vs $ bindMetas f t
        forall h p t'

      let expr = instantiateTele pure vs scope
      expr' <- withVars vs $ bindMetas f expr

      return $ conBranchTyped c vs expr'
  LitBranches lbrs def ->
    LitBranches
      <$> mapM (\(LitBranch l br) -> LitBranch l <$> bindMetas f br) lbrs
      <*> bindMetas f def
