
-----------------------------------------------------------
-- |
-- Module       : Giskard.Calculus.Abstractions
-- Description  : Common Abstraction Implementation
-----------------------------------------------------------
module Giskard.Calculus.Abstractions
    ( Abs (..), Point (..)
    , abstract, abstract1
    , instantiate, instantiate1
    , AbsLike (..)
    , bindAbsSubterms
    ) where

import Control.Monad (ap, liftM)
import Data.Traversable


-- |
-- Abstractions allow terms to contain holes that can be filled in
-- later on.
--
newtype Abs b f a = Abs { unAbs :: f (Point b (f a)) }

-- |
-- A thing that is either a bound hole or a subterm.
--
data Point b a = Bound b | Subterm a

-- |
-- Create an abstraction from a term by replacing points that meet certain
-- criteria with bound holes.
--
abstract :: Monad f => (a -> Maybe b) -> f a -> Abs b f a
abstract test tm = Abs $ do
    a <- tm
    pure $ case test a of
        Just b  -> Bound b
        Nothing -> Subterm (pure a)

-- |
-- Create an abstraction with a single binder for all its holes. This
-- rule corresponds to ordinary lambda abstraction.
--
abstract1 :: (Monad f, Eq a) => a -> f a -> Abs () f a
abstract1 a = abstract (\b -> if a == b then Just () else Nothing)

-- |
-- Instantiate the holes in an abstraction.
--
instantiate :: Monad f => (b -> f a) -> Abs b f a -> f a
instantiate inst (Abs m) =
    m >>= \case
        Bound   b -> inst b
        Subterm a -> a

-- |
-- Instantiate an abstraction with only one hole binder.
--
instantiate1 :: Monad f => f a -> Abs () f a -> f a
instantiate1 a = instantiate (const a)


-- |
-- A thing that's like an abstraction with a strategy for free variable
-- substitutions.
--
class AbsLike t where
    (>>>=) :: Monad f => t f a -> (a -> f c) -> t f c

-- |
-- Apply a substitution to each unbound subterm of an abstraction.
--
bindAbsSubterms :: Monad f => Abs b f a -> (a -> f c) -> Abs b f c
bindAbsSubterms (Abs m) s = Abs $ liftM (fmap (>>= s)) m

instance AbsLike (Abs b) where
    (>>>=) = bindAbsSubterms

-- |
-- Like 'bindAbsSubterms', but insert abstractions rather than concrete
-- terms.
--
bindAbs :: Monad f => Abs b f a -> (a -> Abs b f c) -> Abs b f c
bindAbs (Abs m) f = Abs $ do
    var <- m
    case var of
        Bound   b -> pure $ Bound b
        Subterm a -> a >>= unAbs . f

instance Monad f => Monad (Abs b f) where
    (>>=) = bindAbs

-- Miscellaneous 'Abs' instances.

instance Traversable f => Traversable (Abs b f) where
    traverse f (Abs m) = Abs <$> traverse (traverse $ traverse f) m

instance Functor f => Functor (Abs b f) where
    fmap f (Abs m) = Abs $ fmap (fmap $ fmap f) m

instance Monad f => Applicative (Abs b f) where
    pure  = Abs . pure . Subterm . pure
    (<*>) = ap

instance Foldable f => Foldable (Abs b f) where
    foldMap f (Abs m) = foldMap (foldMap $ foldMap f) m

-- Miscellaneous 'Point' instances.

instance Traversable (Point b) where
    traverse f p = case p of
        Bound   b -> pure $ Bound b
        Subterm a -> Subterm <$> f a

instance Functor (Point b) where fmap = fmapDefault

instance Foldable (Point b) where foldMap = foldMapDefault

