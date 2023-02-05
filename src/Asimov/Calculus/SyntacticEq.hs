
-----------------------------------------------------------
-- |
-- Module       : Asimov.Calculus.SyntacticEq
-- Description  : Syntactic Equality of Proto-Terms
-----------------------------------------------------------
module Asimov.Calculus.SyntacticEq
    ( SynEq (..)
    , synEqTerms
    , synEqAbs, synEqPoints
    ) where

import Asimov.Calculus.Abstractions
import Asimov.Calculus.ProtoTerm


-- |
-- Constructions that have a notion of syntactic equality.
--
class SynEq a where
    synEq :: a -> a -> Bool

-- Some base instances for syntactic equality. 'Text', '()', and 'Int'
-- have trivial syntactic equality thanks to their 'Eq' instances.
instance SynEq ()   where synEq = (==)
instance SynEq Int  where synEq = (==)

-- |
-- Compare two terms in the calculus for syntactic equality.
--
synEqTerms :: SynEq a => ProtoTerm a -> ProtoTerm a -> Bool
synEqTerms t1 t2 = case (t1, t2) of
    (Point point1    , Point point2    )
        -> synEq point1 point2
    -- Term constructor comparisons.
    (Lam dom1 expr1  , Lam dom2 expr2  )
        -> synEq dom1 dom2 && synEq expr1 expr2
    (App fun1 args1  , App fun2 args2  )
        -> synEq fun1 fun2 && synEq args1 args2
    (Let bndr1 body1 , Let bndr2 body2 )
        -> synEq bndr1 bndr2 && synEq body1 body2
    -- Type constructor comparisons.
    (Pi dom1 cod1    , Pi dom2 cod2    )
        -> synEq dom1 dom2 && synEq cod1 cod2
    (Forall dom1 cod1, Forall dom2 cod2)
        -> synEq dom1 dom2 && synEq cod1 cod2
    (Star            , Star            )
        -> True
    -- Anything else is not equal.
    (_               , _               )
        -> False

instance SynEq a => SynEq (ProtoTerm a) where
    synEq = synEqTerms

instance SynEq a => SynEq (ProtoBind a) where
    synEq (Bind u1 ty1) (Bind u2 ty2) = synEq u1 u2 && synEq ty1 ty2
    
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
    => Abs b ProtoTerm a
    -> Abs b ProtoTerm a
    -> Bool
synEqAbs (Abs m1) (Abs m2) = synEqTerms m1 m2

instance (SynEq b, SynEq a) => SynEq (Abs b ProtoTerm a) where
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
