
-----------------------------------------------------------
-- |
-- Module       : Giskard.CoC.Term
-- Description  : Calculus of Constructions Term Syntax
-----------------------------------------------------------
module Giskard.CoC.Term
    ( PreTerm (..)
    , Term' (..), Term, Type
    , mkStar
    , mkVar
    , mkPi, mkLam, mkLet
    , mkApp
    , Name
    ) where

import Data.Text (Text)


-- |
-- Algebra of pre-terms.
--
data PreTerm name aterm atype

    -- | A top-level type of types/telescopes.
    = Star

    | Var name

    -- | Bind a dependent product type @Pi (x : A) -> B x@.
    | Pi name atype atype

    -- | Bind a lambda term @\ (x : A) -> e@.
    | Lam name atype aterm

    -- | Bind a let-binder @let x : A := u in e@.
    | Let name atype aterm aterm

    -- | Apply one term to another.
    | App aterm aterm

    deriving Show


-- |
-- The actual type of terms is the fixed-point of the pre-term algebra type.
--
newtype Term' name = T (PreTerm name (Term' name) (Term' name))
    deriving Show


-- Convenience functions for defining fixpoint terms.

mkStar :: Term' name
mkStar = T Star

mkVar :: name -> Term' name
mkVar name = T (Var name)

mkPi
    :: name         -- ^ Name bound by the pi-abstraction.
    -> Term' name   -- ^ Type of the bound name.
    -> Term' name   -- ^ Type/proposition that the name is bound in.
    -> Term' name
mkPi name dom cod = T (Pi name dom cod)

mkLam
    :: name         -- ^ Name bound by the lambda-abstraction.
    -> Term' name   -- ^ Type of the bound name.
    -> Term' name   -- ^ Term that the name is bound in.
    -> Term' name
mkLam name dom e = T (Lam name dom e)

mkLet
    :: name         -- ^ Name bound by the let-binding.
    -> Term' name   -- ^ Type of the bound name.
    -> Term' name   -- ^ Term to bind to the name.
    -> Term' name   -- ^ Term that the name is bound in.
    -> Term' name
mkLet name ty e body = T (Let name ty e body)

mkApp :: Term' name -> Term' name -> Term' name
mkApp f x = T (App f x)

type Term = Term' Name
type Type = Term
-- TEMPORARY
type Name = Text

