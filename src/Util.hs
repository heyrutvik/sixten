{-# LANGUAGE DeriveFunctor, Rank2Types #-}
module Util where

import Bound
import Bound.Var
import qualified Bound.Scope.Simple as Simple
import Control.Monad.State
import Data.Bifoldable
import Data.Bifunctor
import Data.Foldable
import Data.Hashable
import qualified Data.HashMap.Lazy as HM
import Data.HashSet(HashSet)
import qualified Data.HashSet as HS
import Data.Set(Set)
import qualified Data.Set as S
import Data.String
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Vector(Vector)
import qualified Data.Vector as Vector

type Scope1  = Scope ()
type Literal = Integer

tuck :: Functor f => (a -> Var b a') -> Simple.Scope b f a -> Simple.Scope b f a'
tuck f (Simple.Scope s) = Simple.Scope $ unvar B f <$> s

unusedVar :: (Monad f, Traversable f) => f (Var b a) -> Maybe (f a)
unusedVar = traverse $ unvar (const Nothing) pure

unusedScope :: (Monad f, Traversable f) => Scope b f a -> Maybe (f a)
unusedScope = unusedVar . fromScope

abstractNone :: Monad f => f a -> Scope b f a
abstractNone = Scope . return . F

-- instantiateSome :: Functor f => (b -> Var b' (f a)) -> Scope b f a -> Scope b' f a
-- instantiateSome f (Scope s) = Scope $ fmap g s

instantiateVar :: Functor f => (b -> a) -> Simple.Scope b f a -> f a
instantiateVar f (Simple.Scope s) = unvar f id <$> s

instantiate1Var :: Functor f => a -> Simple.Scope () f a -> f a
instantiate1Var a = instantiateVar (\() -> a)

boundJoin :: (Monad f, Bound t) => t f (f a) -> t f a
boundJoin = (>>>= id)

toSet ::  (Ord a, Foldable f) => f a -> Set a
toSet = foldMap S.singleton

toVector :: Foldable f => f a -> Vector a
toVector = Vector.fromList . toList

toMonoid ::  (Foldable f, Monoid (g a), Applicative g) => f a -> g a
toMonoid = foldMap pure

toHashSet ::  (Eq a, Foldable f, Hashable a) => f a -> HashSet a
toHashSet = foldMap HS.singleton

bimapScope :: Bifunctor f => (x -> x') -> (y -> y') -> Scope b (f x) y -> Scope b (f x') y'
bimapScope f g (Scope s) = Scope $ bimap f (fmap (bimap f g)) s

bifoldMapScope :: (Bifoldable expr, Monoid m)
               => (x -> m) -> (y -> m)
               -> Scope b (expr x) y -> m
bifoldMapScope f g (Scope s) = bifoldMap f (unvar mempty $ bifoldMap f g) s

exposeScope
  :: Applicative expr
  => (forall x. expr x -> expr (Var e x))
  -> Scope b expr a
  -> Scope b expr (Var e a)
exposeScope f (Scope s) = Scope $ fmap (unvar (F . pure . B) id) $ f $ fmap f <$> s

recursiveAbstract :: (Eq v, Foldable t, Functor t, Hashable v, Monad f)
                  => t (v, f v) -> t (Scope Int f v)
recursiveAbstract es = (abstract (`HM.lookup` vs) . snd) <$> es
  where
    vs = HM.fromList $ zip (toList $ fst <$> es) [(0 :: Int)..]

fromText :: IsString a => Text -> a
fromText = fromString . Text.unpack

shower :: (Show a, IsString b) => a -> b
shower = fromString . show

indexed :: Traversable f => f a -> f (Int, a)
indexed x = evalState (traverse go x) 0
  where
    go a = do
      i <- get
      put $! i + 1
      return (i, a)

data Unit a = Unit
  deriving (Functor)
