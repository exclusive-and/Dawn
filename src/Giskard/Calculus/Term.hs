
-----------------------------------------------------------
-- |
-- Module       : Giskard.Calculus.Term
-- Description  : Calculus Term Syntax
-----------------------------------------------------------
module Giskard.Calculus.Term
    ( Term' (..), Type'
    , bindTerm
    , Abs (..), Point (..)
    , abstract, instantiate
    , AbsLike (..)
    , bindAbsSubterms
    , abstract1, instantiate1
    , mkNoAbs
    , mkPi, mkForall, mkLam
    , whnf
    , stripApps
    , Subst'
    , applySubst
    ) where

import              Giskard.Names

import              Control.Monad (ap, liftM)
import              Data.Map (Map)
import qualified    Data.Map as Map
import              Data.Traversable


-----------------------------------------------------------
-- Term Monad
-----------------------------------------------------------

-- |
-- Terms are free monads; extended to support abstraction over typed
-- variables, abstractions of types (pi-types), and let-binders.
-- 
-- See [the bound package](https://hackage.haskell.org/package/bound)
-- for the original implementation of this idea.
-- 
data Term' a
    -- |
    -- A thing that we think is atomic in this term. Might be
    -- a variable or binder, but can also be a subterm during
    -- substitutions.
    = Point  a
    
    -- | A pi-type abstracts over a term in a type.
    | Pi     (Type' a) (Abs () Type' a)
    -- | Normal lambda abstraction over a term in a term.
    | Lam    (Type' a) (Abs () Term' a)

    -- |
    -- 'Forall' terms work identically to 'Pi' terms from the
    -- perspective of the term calculus, but signal to constraint
    -- solvers that they must be solved rather than expecting an
    -- argument there.
    --
    -- Use 'Forall' to model:
    --  * System F type parameters.
    --  * Agda/Idris implicit parameters.
    | Forall (Type' a) (Abs () Type' a)

    -- | @let (x : A) = u in e@ is coded as @(\ (x : A) -> e) u@.
    | Let    (Type' a) (Term' a) (Abs () Term' a)

    -- |
    -- A term applied to a stack of arguments.
    -- 
    -- Invariant: @App f (x:xs) == App (App f [x]) xs@
    | App   (Term' a) [Term' a]
    
    -- | The top-level type of contexts and telescopes.
    | Star

    deriving (Functor, Foldable, Traversable)

type Type' = Term'


instance Applicative Term' where
    pure  = Point
    (<*>) = ap

-- |
-- Recurse over a term to apply a function which substitutes points
-- with new subterms.
-- 
bindTerm :: Term' a -> (a -> Term' c) -> Term' c
bindTerm t s = case t of
    -- Points: replace a point with the term it corresponds to in
    -- the substitution function.
    Point  a       -> s a

    -- Abstractions: apply the substitution to the domain normally,
    -- then go under abstraction to apply the substitution to free
    -- subterms.
    Pi     dom cod -> Pi     (dom >>= s) (cod >>>= s)
    Lam    dom e   -> Lam    (dom >>= s) (e   >>>= s)
    Forall dom cod -> Forall (dom >>= s) (cod >>>= s)

    -- Let: as in the other abstraction terms, but also apply the
    -- substitution to the saved term.
    Let    ty u e  -> Let    (ty  >>= s) (u >>= s) (e >>>= s)

    -- Application: apply the substitution piece-wise on the function
    -- and on each of its arguments.
    App    f e     -> App    (f   >>= s) (map (>>= s) e)

    -- Star: since it isn't a normal point, Star should never be
    -- substituted for anything else.
    Star           -> Star

instance Monad Term' where (>>=) = bindTerm


-----------------------------------------------------------
-- Abstractions and Context Strengthening
-----------------------------------------------------------

-- |
-- An abstraction binds a variable in a term, so that all points
-- in the term are either binders, or subterms with no bound points.
-- It is analogous to the abstraction typing rule:
--
-- @
--        C, x : A |- e : B
--  ------------------------------
--   C |- \ (x : A) -> e : A -> B
-- @
-- 
-- Where all points in @e@ are either @x@, or variables in @C@.
-- 
newtype Abs b f a = Abs { unAbs :: f (Point b (f a)) }

-- |
-- A thing that is either a binder or a subterm.
-- 
data Point b a = Bound b | Subterm a

-- |
-- Abstract a term using the type rule in 'Abs'. Applies a test
-- function to each point, and replaces the point with the appropriate
-- binder when applicable.
-- 
abstract :: Monad f => (a -> Maybe b) -> f a -> Abs b f a
abstract test tm = Abs $ do
    a <- tm
    pure $ case test a of
        Just b  -> Bound b
        Nothing -> Subterm (pure a)

-- |
-- Eliminate an abstraction by replacing bound variables with subterms
-- using a substitution function.
--
instantiate :: Monad f => (b -> f a) -> Abs b f a -> f a
instantiate inst (Abs m) =
    m >>= \case
        Bound   b -> inst b
        Subterm a -> a

-- |
-- A thing that's like an abstraction with a strategy for free variable
-- substitutions.
-- 
class AbsLike t where
    (>>>=) :: Monad f => t f a -> (a -> f c) -> t f c

-- |
-- For points not bound by the abstraction, recursively apply a
-- substitution to their subterms using 'bindTerm'.
-- 
-- For bound points, do nothing.
-- 
bindAbsSubterms :: Monad f => Abs b f a -> (a -> f c) -> Abs b f c
bindAbsSubterms (Abs m) s = Abs $ liftM (fmap (>>= s)) m

instance AbsLike (Abs b) where
    (>>>=) = bindAbsSubterms

-- |
-- Substitution similar to 'bindAbsSubterms'. Substitutes points
-- with entire abstractions, rather than merely terms.
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

instance Foldable (Point b) where
    foldMap = foldMapDefault


-----------------------------------------------------------
-- Convenient Term API
-----------------------------------------------------------

-- |
-- Abstract a term over a single point (using 'Eq', not 'SynEq').
-- 
abstract1 :: (Monad f, Eq a) => a -> f a -> Abs () f a
abstract1 a = abstract (\b -> if a == b then Just () else Nothing)

-- |
-- Instantiate a single bound variable in an abstraction.
-- 
instantiate1 :: Monad f => f a -> Abs () f a -> f a
instantiate1 a = instantiate (const a)

-- |
-- Make an abstraction with no bound subterms. Equivalent to
-- @e ~~> \ _ -> e@.
--
mkNoAbs :: Term' a -> Abs b Term' a
mkNoAbs = Abs . Point . Subterm

-- |
-- Make a pi-type from a type by abstracting over a name.
-- 
mkPi :: Eq a => a -> Type' a -> Type' a -> Type' a
mkPi nm dom cod = Pi dom $ abstract1 nm cod

-- |
-- Make a forall-type from a type by abstracting over a name.
--
mkForall :: Eq a => a -> Type' a -> Type' a -> Type' a
mkForall nm dom cod = Forall dom $ abstract1 nm cod

-- |
-- Make a lambda-term from a term by abstracting over a name.
-- 
mkLam :: Eq a => a -> Type' a -> Term' a -> Term' a
mkLam nm dom tm = Lam dom $ abstract1 nm tm

-- |
-- Evaluate a term to Weak Head-Normal Form (WHNF).
-- 
whnf :: Term' a -> Term' a

whnf (App f (x:xs)) =
    case f of
        Pi _ cod        -> inst cod
        Lam _ e         -> inst e
        Forall _ cod    -> inst cod
        f'              -> App f' (x:xs)
  where
    inst e = whnf $ App (instantiate1 x e) xs

whnf (Let _ u e) = instantiate1 u e

whnf e = e

-- |
-- Strip any applications off the head of a term.
-- 
stripApps :: Term' a -> (Term' a, [Term' a])
stripApps = \case
    App f xs -> (f, xs)
    e        -> (e, [])


-----------------------------------------------------------
-- Simple Pattern Unification of Terms
-----------------------------------------------------------

type Subst' a = Map a (Term' a)

-- |
-- Apply a substitution map to a term.
-- 
-- Works by looking up each of the term's points in the substitution
-- map. Replace each point found in the map by its corresponding term.
-- Otherwise, if we don't find a point, do nothing to it.
-- 
applySubst :: Ord a => Term' a -> Subst' a -> Term' a
applySubst tm subst = do
    a <- tm
    case Map.lookup a subst of
        Just b  -> b
        Nothing -> pure a

