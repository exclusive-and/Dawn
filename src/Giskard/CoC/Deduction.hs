
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------
-- |
-- Module       : Giskard.CoC.Deduction
-- Description  : Judgement and Deduction for the Calculus of Constructions
-----------------------------------------------------------
module Giskard.CoC.Deduction
    ( Judgement' (..), Judgement
    , Sequent' (..), Sequent
    , context, judgement
    , Deduction' (..), Deduction
    , hypotheses, conclusion
    ) where

import              Giskard.CoC.Contexts
import              Giskard.CoC.Term


-- |
-- A judgement @|- J@, on the right side of a turnstile, is a claim
-- that we can deduce within our logical framework.
--
data Judgement' aterm atype
    -- | Claims something is a type: @|- A is a Type@.
    = JIsAType atype
    -- | Claims that two types are definitionally equal: @|- A === B@.
    | JTypeEquality atype atype
    -- | Claims the typing of a term: @|- x : A@.
    | JTyping aterm atype
    -- |
    -- Claims that two terms both of a given type are definitionally
    -- equal: @|- x === y : A@.
    | JEquality aterm aterm atype
    
    deriving Show

type Judgement = Judgement' Term Type


-- |
-- A sequent with antecedent @C@ and judgement @J@ claims that @J@ is
-- typechecked with respect to constants in @C@.
--
data Sequent' ctxt aterm atype
    = Sequent
    { antecedent    :: ctxt
    , judgement     :: Judgement' aterm atype
    }
    deriving Show

type Sequent = Sequent' Context Term Type


-- |
--
--
data Deduction' ctxt aterm atype
    = Deduction
    { hypotheses :: [Sequent' ctxt aterm atype]
    , conclusion :: Sequent' ctxt aterm atype
    }
    deriving Show

type Deduction = Deduction' Context Term Type

