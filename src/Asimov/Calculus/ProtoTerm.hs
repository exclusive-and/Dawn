
-----------------------------------------------------------
-- |
-- Module       : Asimov.Calculus.ProtoTerm
-- Description  : Prototypical Term Syntax of the Lambda Cube
-----------------------------------------------------------
module Asimov.Calculus.ProtoTerm
    ( ProtoTerm (..), ProtoType
    , ProtoBind (..)
    , bindTerm
    , mkNoAbs, mkPi, mkForall, mkLam
    , whnf
    , stripApps
    ) where

import Asimov.Calculus.Abstractions
import Asimov.Pretty

import Control.Monad (ap)
import Data.Text (Text, intercalate)


-----------------------------------------------------------
-- Prototypical Term Monad
-----------------------------------------------------------

-- |
-- Terms are free monads. Extended with constructors for types,
-- and type-annotated domains in abstractions for type inference.
-- 
-- See [the bound package](https://hackage.haskell.org/package/bound)
-- for the original implementation of this idea.
-- 
data ProtoTerm a

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
    | Lam    (ProtoType a) (Abs () ProtoTerm a)
    
    -- |
    -- A term applied to a stack of arguments outside-in.
    --
    -- Invariant: @App f (x:xs) == App (App f [x]) xs@
    | App    (ProtoTerm a) [ProtoTerm a]
    
    -- | @let (x : A) = u in e@ is coded as @(\ (x : A) -> e) u@.
    | Let    (ProtoBind a) (Abs () ProtoTerm a)
    
    -- The type-level constructors.
    
    -- | Types of λ-abstractions. Abstracts over a term in a type.
    | Pi     (ProtoType a) (Abs () ProtoType a)

    -- |
    -- 'Forall' types are a lot-like 'Pi' types. But instead of
    -- instantiating the codomain with a visible argument, we depend
    -- on the inference engine to deduce which terms should be used.
    --
    -- Use 'Forall' to model:
    --  * System F type parameters.
    --  * Agda/Idris implicit parameters.
    | Forall (ProtoType a) (Abs () ProtoType a)
    
    -- |
    -- The inaccessible type of types. Anything that's too high up
    -- to be typed in the normal language - e.g. the Tarski universe
    -- operators - should have type Star.
    | Star

    deriving
        ( Foldable
        , Functor       -- ^ Needed in 'bindAbsSubterms'.
        , Traversable
        )

-- |
-- Like other dependently typed languages (e.g. Idris, Agda, Coq, etc),
-- types are also terms.
-- 
type ProtoType = ProtoTerm

-- |
-- Typed binding of a term. We can omit names from these binders
-- since 'Abs' will fill in the binding site automagically for us.
-- 
data ProtoBind a = Bind (ProtoTerm a) (ProtoType a)
    deriving (Functor, Foldable, Traversable)


instance Applicative ProtoTerm where
    pure  = Point
    (<*>) = ap

-- |
-- Recurse over a term to apply a function which substitutes points
-- with new subterms.
-- 
bindTerm :: ProtoTerm a -> (a -> ProtoTerm c) -> ProtoTerm c
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

instance Monad ProtoTerm where (>>=) = bindTerm


-----------------------------------------------------------
-- Convenient Term API
-----------------------------------------------------------

-- |
-- Make an abstraction with no bound subterms. Equivalent to
-- @e ~~> \ _ -> e@.
--
mkNoAbs :: ProtoTerm a -> Abs b ProtoTerm a
mkNoAbs = Abs . Point . Subterm

-- |
-- Make a pi-type from a type by abstracting over a name.
-- 
mkPi :: Eq a => a -> ProtoType a -> ProtoType a -> ProtoType a
mkPi nm dom cod = Pi dom $ abstract1 nm cod

-- |
-- Make a forall-type from a type by abstracting over a name.
--
mkForall :: Eq a => a -> ProtoType a -> ProtoType a -> ProtoType a
mkForall nm dom cod = Forall dom $ abstract1 nm cod

-- |
-- Make a lambda-term from a term by abstracting over a name.
-- 
mkLam :: Eq a => a -> ProtoType a -> ProtoTerm a -> ProtoTerm a
mkLam nm dom tm = Lam dom $ abstract1 nm tm

-- |
-- Evaluate a term to Weak Head-Normal Form (WHNF).
-- 
whnf :: ProtoTerm a -> ProtoTerm a

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
stripApps :: ProtoTerm a -> (ProtoTerm a, [ProtoTerm a])
stripApps = \case
    App f xs -> (f, xs)
    e        -> (e, [])


-----------------------------------------------------------
-- Term Pretty-Printing
-----------------------------------------------------------

-- |
-- Prettyprint a term whose points have already been prettyprinted.
-- 
pprTerm' :: Bool -> Int -> ProtoTerm Text -> Text
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
pprTerm :: Pretty a => ProtoTerm a -> Text
pprTerm = pprTerm' False 0 . (pure . pretty =<<)

instance Pretty a => Pretty (ProtoTerm a) where pretty = pprTerm

-- |
-- Prettyprint a binder.
-- 
pprBound :: Int -> Text
pprBound boundId = "_bound_" <> pretty boundId

-- |
-- Prettyprint a term under another level of abstraction.
-- 
pprAbs :: Int -> Abs () ProtoTerm Text -> Text
pprAbs bindLvl tm =
    pprTerm' False (bindLvl + 1) $ instantiate1 (pure $ pprBound bindLvl) tm

-- |
-- Prettyprint a lambda expression.
-- 
pprLam :: Int -> ProtoType Text -> Abs () ProtoTerm Text -> Text
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
pprPi :: Int -> ProtoType Text -> Abs () ProtoType Text -> Text
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
pprForall :: Int -> ProtoType Text -> Abs () ProtoType Text -> Text
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
pprLet :: Int -> ProtoBind Text -> Abs () ProtoTerm Text -> Text
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
pprApp :: Int -> ProtoTerm Text -> [ProtoTerm Text] -> Text
pprApp bindLvl f xs =
  let
    f'  = pprTerm' True bindLvl f
    xs' = map (pprTerm' True bindLvl) xs
  in
    f' <> " " <> intercalate " " xs'


