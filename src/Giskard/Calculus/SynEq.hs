
-----------------------------------------------------------
-- |
-- Module       : Giskard.Calculus.SynEq
-- Description  : Syntactic Equality of Terms
-----------------------------------------------------------
module Giskard.Calculus.SynEq
    ( SynEq (..)
    , synEqTerms
    , synEqAbs, synEqPoints
    ) where

import Giskard.Calculus.Term

import Giskard.Names


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
    (Point tm1       , Point tm2       ) -> synEq tm1 tm2
    (Pi dom1 cod1    , Pi dom2 cod2    )
        -> synEq dom1 dom2 && synEq cod1 cod2
    (Lam dom1 tm1    , Lam dom2 tm2    )
        -> synEq dom1 dom2 && synEq tm1 tm2
    (Let ty1 u1 e1   , Let ty2 u2 e2   )
        -> synEq ty1 ty2 && synEq u1 u2 && synEq e1 e2
    (Forall dom1 cod1, Forall dom2 cod2)
        -> synEq dom1 dom2 && synEq cod1 cod2
    (App f1 x1       , App f2 x2       )
        -> synEq f1 f2 && synEq x1 x2
    (Star            , Star            ) -> True
    (_               , _               ) -> False

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
