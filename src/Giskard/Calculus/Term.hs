
-----------------------------------------------------------
-- |
-- Module       : Giskard.Calculus.Term
-- Description  : Calculus Term Syntax
-----------------------------------------------------------
module Giskard.Calculus.Term
    ( Term' (..), Type'
    , Bind' (..)
    , bindTerm
    , Abs (..), Point (..)
    , abstract, instantiate
    , AbsLike (..)
    , bindAbsSubterms
    , abstract1, instantiate1
    , mkNoAbs, mkPi, mkForall, mkLam
    , whnf
    , stripApps
    ) where

import Giskard.Pretty
    
import Control.Monad (ap, liftM)
import Data.Traversable
import Data.Text (Text, intercalate)


-----------------------------------------------------------
-- Term Monad
-----------------------------------------------------------

-- |
-- Terms are free monads. Extended with constructors for types,
-- and type-annotated domains in abstractions for type inference.
-- 
-- See [the bound package](https://hackage.haskell.org/package/bound)
-- for the original implementation of this idea.
-- 
data Term' a

    -- The term-level constructors.

    -- |
    -- A thing that we think is atomic in this term.
    -- 
    -- What is atomic depends on what the term represents. In the
    -- simple case, variables and literals are atomic. In an
    -- abstraction, the atomic points are binding sites and free
    -- subterms.
    = Point  a
    
    -- |
    -- λ-abstractions: abstract over a term in a term.
    -- 
    -- Equip it with an instantiation rule that fills in binding sites
    -- to make a function.
    | Lam    (Type' a) (Abs () Term' a)
    
    -- |
    -- A term applied to a stack of arguments outside-in.
    --
    -- Invariant: @App f (x:xs) == App (App f [x]) xs@
    | App    (Term' a) [Term' a]
    
    -- | @let (x : A) = u in e@ is coded as @(\ (x : A) -> e) u@.
    | Let    (Bind' a) (Abs () Term' a)
    
    
    -- The type-level constructors.
    
    -- | Types of λ-abstractions. Abstracts over a term in a type.
    | Pi     (Type' a) (Abs () Type' a)

    -- |
    -- 'Forall' types are a lot-like 'Pi' types. But instead of
    -- instantiating the codomain with a visible argument, we depend
    -- on the inference engine to deduce which terms should be used.
    --
    -- Use 'Forall' to model:
    --  * System F type parameters.
    --  * Agda/Idris implicit parameters.
    | Forall (Type' a) (Abs () Type' a)
    
    -- |
    -- The inaccessible type of types. Anything that's too high up
    -- to be typed in the normal language - e.g. the Tarski universe
    -- operators - should have type Star.
    | Star

    deriving
        ( Functor       -- ^ Needed in 'bindAbsSubterms'.
        , Foldable
        , Traversable
        )

-- |
-- Like other dependently typed languages (e.g. Idris, Agda, Coq, etc),
-- types are also terms.
-- 
type Type' = Term'

-- |
-- Typed binding of a term. We can omit names from these binders
-- since 'Abs' will fill in the binding site automagically for us.
-- 
data Bind' a = Bind (Term' a) (Type' a)
    deriving (Functor, Foldable, Traversable)


instance Applicative Term' where
    pure  = Point
    (<*>) = ap

-- |
-- Recurse over a term to apply a function which substitutes points
-- with new subterms.
-- 
bindTerm :: Term' a -> (a -> Term' c) -> Term' c
bindTerm t0 s = go t0 where
    go t = case t of
        -- Points: replace a point with the term it corresponds to in
        -- the substitution function.
        Point  a       -> s a

        -- Abstractions: apply the substitution to the domain normally,
        -- then go under abstraction to apply the substitution to free
        -- subterms.
        Lam    dom e   -> Lam    (go dom) (e   >>>= s)
        Pi     dom cod -> Pi     (go dom) (cod >>>= s)
        Forall dom cod -> Forall (go dom) (cod >>>= s)

        -- Let: as in the other abstraction terms, but also apply the
        -- substitution to the saved term.
        Let    bndr e  -> Let    (goBndr bndr) (e >>>= s)

        -- Application: apply the substitution piece-wise on the function
        -- and on each of its arguments.
        App    f e     -> App    (go f) (map go e)

        -- Star: since it isn't a normal point, Star should never be
        -- substituted for anything else.
        Star           -> Star
    
    goBndr (Bind u ty) = Bind (go u) (go ty)

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
        Lam _ e -> inst e
        f'      -> App f' (x:xs)
  where
    inst e = whnf $ App (instantiate1 x e) xs

whnf (Let (Bind u _) e) = instantiate1 u e

whnf e = e

-- |
-- Strip any applications off the head of a term.
-- 
stripApps :: Term' a -> (Term' a, [Term' a])
stripApps = \case
    App f xs -> (f, xs)
    e        -> (e, [])


-----------------------------------------------------------
-- Term Pretty-Printing
-----------------------------------------------------------

-- |
-- Prettyprint a term whose points have already been prettyprinted.
-- 
pprTerm' :: Bool -> Int -> Term' Text -> Text
pprTerm' shouldParen bindLvl tm = case tm of
    -- Points are already pretty-printed, so just emit them.
    Point  point   -> point
    
    Lam    dom e   -> parens $ pprLam bindLvl dom e
    Pi     dom cod -> parens $ pprPi bindLvl dom cod
    Forall dom cod -> parens $ pprForall bindLvl dom cod
    Let    bndr e  -> parens $ pprLet bindLvl bndr e
    
    App    f xs    -> parens $ pprApp bindLvl f xs
    
    Star           -> "*"
  where
    parens s = if shouldParen then "(" <> s <> ")" else s

-- |
-- Prettyprint a term after prettyprinting all its points.
-- Processing the points first allows us to prettyprint any term as
-- long as we have a point prettyprinter instance.
-- 
pprTerm :: Ppr a => Term' a -> Text
pprTerm = pprTerm' False 0 . (pure . ppr =<<)

instance Ppr a => Ppr (Term' a) where ppr = pprTerm

-- |
-- Prettyprint a binder.
-- 
pprBound :: Int -> Text
pprBound boundId = "_bound_" <> ppr boundId

-- |
-- Prettyprint a term under another level of abstraction.
-- 
pprAbs :: Int -> Abs () Term' Text -> Text
pprAbs bindLvl tm =
    pprTerm' False (bindLvl + 1) $ instantiate1 (pure $ pprBound bindLvl) tm

-- |
-- Prettyprint a lambda expression.
-- 
pprLam :: Int -> Type' Text -> Abs () Term' Text -> Text
pprLam bindLvl dom e =
  let
    x'   = pprBound bindLvl
    dom' = pprTerm' False bindLvl dom
    e'   = pprAbs bindLvl e
  in
    "Lam (" <> x' <> " : " <> dom' <> ") -> " <> e'

-- |
-- Prettyprint a pi-type.
-- 
pprPi :: Int -> Type' Text -> Abs () Type' Text -> Text
pprPi bindLvl dom cod =
  let
    x'   = pprBound bindLvl
    dom' = pprTerm' False bindLvl dom
    cod' = pprAbs bindLvl cod
  in
    "Pi (" <> x' <> " : " <> dom' <> ") -> " <> cod'

-- |
-- Prettyprint a forall-type.
-- 
pprForall :: Int -> Type' Text -> Abs () Type' Text -> Text
pprForall bindLvl dom cod =
  let
    x'   = pprBound bindLvl
    dom' = pprTerm' False bindLvl dom
    cod' = pprAbs bindLvl cod
  in
    "Forall (" <> x' <> " : " <> dom' <> ") -> " <> cod'

-- |
-- Prettyprint a let-expression.
-- 
pprLet :: Int -> Bind' Text -> Abs () Term' Text -> Text
pprLet bindLvl (Bind u ty) e =
  let
    x'  = pprBound bindLvl
    ty' = pprTerm' False bindLvl ty
    u'  = pprTerm' False bindLvl u
    e'  = pprAbs bindLvl e
  in
    "Let (" <> x' <> " : " <> ty' <> ") := " <> u' <> " In " <> e' 

-- |
-- Prettyprint an application.
-- 
pprApp :: Int -> Term' Text -> [Term' Text] -> Text
pprApp bindLvl f xs =
  let
    f'  = pprTerm' True bindLvl f
    xs' = map (pprTerm' True bindLvl) xs
  in
    f' <> " " <> intercalate " " xs'


