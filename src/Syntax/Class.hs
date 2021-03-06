{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, GADTs, OverloadedStrings, RankNTypes #-}
module Syntax.Class where

import Protolude

import Bound
import Bound.Scope
import Control.Monad.Morph
import Data.Bitraversable
import Data.Functor.Classes
import Data.Vector(Vector)

import Error
import Pretty
import Syntax.Annotation
import Syntax.GlobalBind
import Syntax.Name
import Syntax.Telescope
import TypedFreeVar
import Util

-------------------------------------------------------------------------------
-- * Class definitions
data ClassDef typ v = ClassDef
  { classParams :: Telescope Plicitness typ v
  , classMethods :: [Method (Scope TeleVar typ v)]
  } deriving (Foldable, Functor, Show, Traversable)

classDef
  :: Monad typ
  => Vector (FreeVar Plicitness typ)
  -> [Method (typ (FreeVar Plicitness typ))]
  -> ClassDef typ (FreeVar Plicitness typ)
classDef vs ms = ClassDef (varTelescope vs) $ fmap abstr <$> ms
  where
    abstr = abstract $ teleAbstraction vs

bimapClassDef
  :: Bifunctor typ
  => (a -> a')
  -> (b -> b')
  -> ClassDef (typ a) b
  -> ClassDef (typ a') b'
bimapClassDef f g (ClassDef ps ms) = ClassDef (bimapTelescope f g ps) $ fmap (bimapScope f g) <$> ms

bitraverseClassDef
  :: (Bitraversable typ, Applicative f)
  => (a -> f a')
  -> (b -> f b')
  -> ClassDef (typ a) b
  -> f (ClassDef (typ a') b')
bitraverseClassDef f g (ClassDef ps ms) = ClassDef <$> bitraverseTelescope f g ps <*> traverse (traverse $ bitraverseScope f g) ms

transverseClassDef
  :: (Traversable typ, Monad f)
  => (forall r. typ r -> f (typ' r))
  -> ClassDef typ a
  -> f (ClassDef typ' a)
transverseClassDef f (ClassDef ps ms) = ClassDef <$> transverseTelescope f ps <*> traverse (traverse $ transverseScope f) ms

instance Bound ClassDef where
  ClassDef ps ms >>>= f = ClassDef (ps >>>= f) $ fmap (>>>= f) <$> ms

instance GBound ClassDef where
  gbound f (ClassDef ps ms) = ClassDef (gbound f ps) $ fmap (gbound f) <$> ms

instance MFunctor ClassDef where
  hoist f (ClassDef ps ms) = ClassDef (hoist f ps) $ fmap (hoistScope f) <$> ms

methodLocNames :: ClassDef typ v -> [(Name, SourceLoc)]
methodLocNames = fmap (\(Method name loc _) -> (name, loc)) . classMethods

data Method expr = Method
  { methodName :: !Name
  , methodLoc :: !SourceLoc
  , methodExpr :: expr
  } deriving (Foldable, Functor, Show, Traversable)

instance (Eq1 typ, Monad typ, Pretty (typ v), v ~ Doc) => PrettyNamed (ClassDef typ v) where
  prettyNamed name (ClassDef ps ms) = "class" <+> name <+> withTeleHints ps (\ns ->
    let inst = instantiateTele (pure . fromName) ns in
        prettyTeleVarTypes ns ps <+> "where" <$$>
          indent 2 (vcat (prettyMethodDecl . fmap inst <$> ms))
    )

prettyMethodDecl :: Pretty typ => Method typ -> PrettyDoc
prettyMethodDecl (Method n _ t) = prettyM n <+> ":" <+> prettyM t

prettyMethodDef :: PrettyNamed expr => Method expr -> PrettyDoc
prettyMethodDef (Method n _ e) = prettyNamed (prettyM n) e
