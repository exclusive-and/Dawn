
-----------------------------------------------------------
-- |
-- Module       : Giskard.CoC.Term
-- Description  : Calculus of Constructions Term Syntax
-----------------------------------------------------------
module Giskard.CoC.Term
    ( Term (..), Type
    , Name
    ) where

import Data.List (zipWith)
import Data.Text (Text)


data Term
    = Var   Name            -- ^ Named free variable.
    | Rel   Int             -- ^ De Bruijn indexed variable - free or bound.
    | Pi    Type Type       -- ^ Pi-type binder @(x : A) -> B x@.
    | Lam   Type Term       -- ^ Lambda binder @\ (x : A) -> e@.
    | Let   Type Term Term  -- ^ Let-in binder @let x : A = e in b@.
    | App   Term [Term]     -- ^ Apply a term to a spine.
    | Star                  -- ^ Top-level type of telescopes.
    deriving Show


type Type = Term    

-- TEMPORARY
type Name = Text


class SynEq a where
    synEq :: a -> a -> Bool

-- |
-- Compare two terms for syntactic equality.
-- 
synEqTerms :: Term -> Term -> Bool
synEqTerms t1 t2 = case (t1, t2) of
    (Var nm1      , Var nm2      ) -> nm1 == nm2
    (Rel id1      , Rel id2      ) -> id1 == id2
    (Pi dom1 cod1 , Pi dom2 cod2 )
        -> synEq dom1 dom2 && synEq dom2 cod2
    (Lam dom1 tm1 , Lam dom2 tm2 )
        -> synEq dom1 dom2 && synEq tm1 tm2
    (Let ty1 u1 e1, Let ty2 u2 e2)
        -> synEq ty1 ty2 && synEq u1 u2 && synEq e1 e2
    (App f1 x1    , App f2 x2    )
        -> synEq f1 f2 && synEq x1 x2
    (Star         , Star         ) -> True
    (_            , _            ) -> False

instance SynEq Term where
    synEq = synEqTerms

instance SynEq a => SynEq [a] where
    synEq vs1 vs2
        | length vs1 == length vs2 = and $ zipWith synEq vs1 vs2
        | otherwise                = False
