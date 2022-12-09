
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------
-- |
-- Module       : Giskard.Syntax
-- Description  : Abstract Syntax of the Giskard Language.
-----------------------------------------------------------
module Giskard.Syntax
    ( Judgement (..)
    , Deduction (..)
    , Term' (..), Binder (..)
    , Term, Type
    ) where

import              Control.DeepSeq (NFData)
import              Data.IntMap (IntMap)
import              Data.HashMap.Strict (HashMap)
import qualified    Data.List as List
import              Data.Text (Text)
import              GHC.Generics (Generic)


-- |
-- Judgements are claims about formation and equality of types, and
-- the typing of terms and equality of terms.
--
data Judgement aterm atype

    -- | Claims the formation of a new type.
    = JTypeFormation atype

    -- | Claims that two types are definitionally equal.
    | JTypeEquality atype atype

    -- | Claims the typing of a term.
    | JTyping aterm atype

    -- | Claims that two terms are definitionally equal.
    | JEqual aterm aterm atype
    
    deriving (Show, Generic)

-- |
-- A deduction is a relationship of lists of claims. If we assume the
-- claims listed in the hypotheses, then we can deduce the claims listed
-- as conclusions.
--
data Deduction aterm atype = Deduction
    { hypotheses  :: [Judgement aterm atype]
    , conclusions :: [Judgement aterm atype]
    }
    deriving Show


-- |
-- Algebra of typed terms.
--
data Term' name
    = Var name

    -- | Bind a term to a name in the body of the bind.
    | Bind
        name                    -- ^ Name to bind to.
        (Binder (Term' name))   -- ^ Binder data.
        (Term' name)            -- ^ Sub-term with bound term.

    -- | Apply one term to another.
    | App
        (Term' name)
        (Term' name)

    deriving Show

-- |
-- Information about what and how exactly we're binding some term.
--
data Binder bindTy
    = Lam bindTy        -- ^ Bind with @\(x : A) -> y : B@
    | Pi  bindTy bindTy -- ^ Bind with @Pi (x : A) -> y : B x@
    | Let bindTy bindTy
    deriving Show


type Term = Term' Name
type Type = Term
-- TEMPORARY
type Name = Text



