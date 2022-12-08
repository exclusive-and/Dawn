
-----------------------------------------------------------
-- |
-- Module       : Dawn.Syntax
-- Description  : Abstract Syntax of the Dawn Language.
-----------------------------------------------------------
module Dawn.Syntax
    ( Judgement (..), Deduction (..)
    , Term' (..), Binder (..), Term, Type
    ) where


-- |
-- Judgements are claims about types and terms with respect to their type.
--
data Judgement
    -- |
    -- Claims the formation of a new type.
    = JFormation Type

    -- |
    -- Claims that two types are definitionally equal.
    | JTypesEqual Type Type

    -- |
    -- Claims the typing of a term.
    | JType Term Type

    -- |
    -- Claims that two terms are definitionally equal.
    | JEqual Term Term Type


-- |
-- A deduction is a relationship of lists of claims. If we assume the claims
-- listed in the hypotheses, then we can deduce the claims listed as
-- conclusions.
--
data Deduction = Deduction
    { hypotheses    :: [Judgement]
    , conclusions   :: [Judgement]
    }


data Term' name
    = Var Int

    -- |
    -- Bind a term of some type to a name in the body sub-term.
    | Bind
        name                    -- ^ Name to bind to.
        (Binder (Term' name))   -- ^ Binder data.
        (Term' name)            -- ^ Sub-term with bound term.

    -- |
    -- Apply one term to another.
    | App
        (Term' name)
        (Term' name)

-- |
-- Information about what and how exactly we're binding some term.
--
data Binder bindTy
    = Lam bindTy
    | Pi  bindTy bindTy
    | Let bindTy bindTy


type Term = Term' Name
type Type = Term
