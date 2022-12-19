
-----------------------------------------------------------
-- |
-- Module       : Giskard.CoC.Term
-- Description  : Calculus of Constructions Term Syntax
-----------------------------------------------------------
module Giskard.CoC.Term
    ( Term' (..), Type'
    , bindTerm
    , Abs (..), AbsLike (..)
    , abstract, instantiate
    , bindAbsSubterms
    , Point (..)
    , Term, Type, Name
    ) where

import Control.Monad (ap, liftM)
import Data.Text (Text)
import GHC.Generics (Generic)


-----------------------------------------------------------
-- Term Monad
-----------------------------------------------------------

-- |
-- Terms are free monads, extended to support abstractions over
-- typed terms, abstractions of types (pi-types), and let-binders.
-- 
-- See [the bound package](https://hackage.haskell.org/package/bound)
-- for the original implementation of this idea.
-- 
data Term' a
    -- |
    -- A thing that we think is a constant in this context. Might be
    -- an identifier or binder, but can also be a subterm during
    -- substitution.
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
    deriving Functor
    
type Type' = Term'

-- |
-- Recurse over a term to apply a function which substitutes constants
-- for new subterms.
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
-- An abstraction is a term whose constants are either bound by the
-- abstraction, or are subterms from the external context. Analogous
-- to the abstraction typing rule:
--
-- @
--    C, x : A |- e : B
--  ---------------------
--   C |- \ (x : A) -> e
-- @
-- 
-- Notice that constants in @e@ are either @x@, or variables with
-- type information in @C@.
-- 
newtype Abs b f a = Abs { unAbs :: f (Point b (f a)) }
    deriving Generic
    
instance Functor f => Functor (Abs b f) where
    fmap f (Abs m) = Abs $ fmap (fmap (fmap f)) m

-- |
-- Abstract a term using the type rule in 'Abs'. Replaces a subterm
-- in context that satisfies the given predicate with a bound constant.
-- 
abstract :: Monad f => (a -> Maybe b) -> f a -> Abs b f a
abstract test tm = Abs $ do
    a <- tm
    pure $ case test a of
        Just b  -> Bound b
        Nothing -> Subterm (pure a)

-- |
-- Instantiate a bound constant in an abstraction to eliminate the
-- abstraction.
--
instantiate :: Monad f => (b -> f a) -> Abs b f a -> f a
instantiate inst (Abs m) = do
    var <- m
    case var of
        Bound   b -> inst b
        Subterm a -> a
        
-- |
-- A thing that's like an abstraction with a strategy for free variable
-- substitutions.
-- 
class AbsLike t where
    (>>>=) :: Monad f => t f a -> (a -> f c) -> t f c

-- |
-- For subterms not bound by the abstraction, recursively apply a
-- substitution using 'bindTerm'. For bound subterms, do nothing.
-- 
bindAbsSubterms :: Monad f => Abs b f a -> (a -> f c) -> Abs b f c
bindAbsSubterms (Abs m) s = Abs $ liftM (fmap (>>= s)) m
    
instance AbsLike (Abs b) where (>>>=) = bindAbsSubterms


-----------------------------------------------------------
-- Bound and Free Subterms
-----------------------------------------------------------
    
-- |
-- A thing that is either a binder or a subterm in the outer context.
-- 
data Point b a
    = Bound     b
    | Subterm   a

instance Functor (Point b) where
    fmap f p = case p of
        Bound   b -> Bound b
        Subterm a -> Subterm $ f a

        
-----------------------------------------------------------
-- Convenient Term API
-----------------------------------------------------------

type Term = Term' Name
type Type = Term
-- TEMPORARY
type Name = Text


-----------------------------------------------------------
-- Syntactic Equality
-----------------------------------------------------------

class SynEq a where
    synEq :: a -> a -> Bool
    
-- |
-- Compare two terms for syntactic equality.
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

instance SynEq Text where
    synEq = (==)
    
instance SynEq a => SynEq [a] where
    synEq vs1 vs2
        | length vs1 == length vs2 = and $ zipWith synEq vs1 vs2
        | otherwise                = False


instance (SynEq b, SynEq a) => SynEq (Abs b Term' a) where
    synEq (Abs m1) (Abs m2) = synEq m1 m2

instance (SynEq b, SynEq a) => SynEq (Point b a) where
    synEq p1 p2 = case (p1, p2) of
        (Bound b1  , Bound b2  ) -> synEq b1 b2
        (Subterm a1, Subterm a2) -> synEq a1 a2
        (_         , _         ) -> False

instance SynEq () where synEq = (==)
        
