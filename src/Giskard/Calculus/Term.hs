
-----------------------------------------------------------
-- |
-- Module       : Giskard.Calculus.Term
-- Description  : Calculus Term Syntax
-----------------------------------------------------------
module Giskard.Calculus.Term
    ( Term' (..), Type'
    , bindTerm
    , Abs (..)
    , abstract, instantiate
    , AbsLike (..)
    , bindAbsSubterms
    , Point (..)
    , Term, Type
    , abstract1, instantiate1
    , mkPi, mkLam
    , whnf
    , SynEq (..)
    , synEqTerms
    , synEqAbs, synEqPoints
    , Subst
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
    = Point a
    
    -- | A pi-type abstracts over a term in a type.
    | Pi    (Type' a) (Abs () Type' a)
    -- | Normal lambda abstraction over a term in a term.
    | Lam   (Type' a) (Abs () Term' a)
    -- | @let (x : A) = u in e@ is coded as @(\ (x : A) -> e) u@.
    | Let   (Type' a) (Term' a) (Abs () Term' a)
    -- | A term applied to a stack of arguments.
    | App   (Term' a) [Term' a]
    -- | The top-level type of contexts and telescopes.
    | Star
    deriving (Functor, Foldable, Traversable)

type Type' = Term'

-- |
-- Recurse over a term to apply a function which substitutes points for
-- new subterms.
-- 
bindTerm :: Term' a -> (a -> Term' c) -> Term' c
bindTerm t s = case t of
    Point a       -> s a
    Pi    dom cod -> Pi  (dom >>= s) (cod >>>= s)
    Lam   dom e   -> Lam (dom >>= s) (e   >>>= s)
    Let   ty u e  -> Let (ty  >>= s) (u >>= s) (e >>>= s)
    App   f e     -> App (f   >>= s) (map (>>= s) e)
    Star          -> Star

instance Monad Term' where (>>=) = bindTerm

instance Applicative Term' where
    pure  = Point
    (<*>) = ap


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


-----------------------------------------------------------
-- Bound and Free Subterms
-----------------------------------------------------------
    
-- |
-- A thing that is either a binder or a subterm.
-- 
data Point b a = Bound b | Subterm a

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

type Term = Term' Name
type Type = Term

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
-- Make a pi-type from a type by abstracting over a name.
-- 
mkPi :: Eq a => a -> Type' a -> Type' a -> Type' a
mkPi nm dom cod = Pi dom $ abstract1 nm cod

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
        Pi _ cod -> inst cod
        Lam _ e  -> inst e
        f'       -> App f' (x:xs)
  where
    inst e = whnf $ App (instantiate1 x e) xs
        
whnf e = e


-----------------------------------------------------------
-- Syntactic Equality
-----------------------------------------------------------

-- |
-- Constructions that have a notion of syntactic equality.
-- 
class SynEq a where
    synEq :: a -> a -> Bool

-- Some base instances for syntactic equality. 'Text', '()', and 'Int'
-- have trivial syntactic equality thanks to their 'Eq' instances.
instance SynEq ()   where synEq = (==)
instance SynEq Name where synEq = (==)

-- |
-- Compare two terms in the calculus for syntactic equality.
-- 
synEqTerms :: SynEq a => Term' a -> Term' a -> Bool
synEqTerms t1 t2 = case (t1, t2) of
    (Point tm1    , Point tm2    ) -> synEq tm1 tm2
    (Pi dom1 cod1 , Pi dom2 cod2 )
        -> synEq dom1 dom2 && synEq cod1 cod2
    (Lam dom1 tm1 , Lam dom2 tm2 )
        -> synEq dom1 dom2 && synEq tm1 tm2
    (Let ty1 u1 e1, Let ty2 u2 e2)
        -> synEq ty1 ty2 && synEq u1 u2 && synEq e1 e2
    (App f1 x1    , App f2 x2    )
        -> synEq f1 f2 && synEq x1 x2
    (Star         , Star         ) -> True
    (_            , _            ) -> False

instance SynEq a => SynEq (Term' a) where
    synEq = synEqTerms

instance SynEq a => SynEq [a] where
    synEq vs1 vs2
        | length vs1 == length vs2 = and $ zipWith synEq vs1 vs2
        | otherwise                = False

-- |
-- Abstractions have trivial syntactic equality by the syntactic
-- equality of their underlying terms.
-- 
synEqAbs
    :: (SynEq b, SynEq a)
    => Abs b Term' a
    -> Abs b Term' a
    -> Bool
synEqAbs (Abs m1) (Abs m2) = synEqTerms m1 m2

instance (SynEq b, SynEq a) => SynEq (Abs b Term' a) where
    synEq = synEqAbs

-- |
-- Points have syntactic equality if both bound variables and free
-- subterms both have syntactic equality.
-- 
synEqPoints :: (SynEq b, SynEq a) => Point b a -> Point b a -> Bool
synEqPoints p1 p2 = case (p1, p2) of
    (Bound   b1, Bound   b2) -> synEq b1 b2
    (Subterm a1, Subterm a2) -> synEq a1 a2
    (_         , _         ) -> False

instance (SynEq b, SynEq a) => SynEq (Point b a) where
    synEq = synEqPoints


-----------------------------------------------------------
-- Simple Pattern Unification of Terms
-----------------------------------------------------------

type Subst a = Map a (Term' a)

-- |
-- Apply a substitution map to a term.
-- 
-- Works by looking up each of the term's points in the substitution
-- map. Replace each point found in the map by its corresponding term.
-- Otherwise, if we don't find a point, do nothing to it.
-- 
applySubst :: Ord a => Term' a -> Subst a -> Term' a
applySubst tm subst = do
    a <- tm
    case Map.lookup a subst of
        Just b  -> b
        Nothing -> pure a

