
-----------------------------------------------------------
-- |
-- Module       : Giskard.CoC.Deduction
-- Description  : Knowledge Representation and Reasoning in Giskard
-----------------------------------------------------------
module Giskard.CoC.Deduction
    ( Judgement (..), Sequent (..)
    , context, judgement
    , Deduction (..)
    , hypotheses, conclusion
    ) where

import Giskard.CoC.Contexts
import Giskard.CoC.Term


-- |
-- Judgements are the most basic things that Giskard can know about.
--
data Judgement

    -- | Claims something is a type: @|- A is a Type@.
    = JIsAType
        Type         -- ^ @A@

    -- |
    -- Claims that two types are definitionally equal: @|- A === B@.
    -- This is the only way that we permit an interchange of types: a
    -- term may only have one type modulo this equality.
    | JTypesAreEqual
        Type         -- ^ @A@
        Type         -- ^ @B@

    -- | Claims the typing of a term: @|- x : A@.
    | JOfType
        Term         -- ^ @x@
        Type         -- ^ @A@

    -- |
    -- Claims that two terms both of a given type are definitionally
    -- equal: @|- x === y : A@.
    | JAreEqual
        Term         -- ^ @x@
        Term         -- ^ @y@
        Type         -- ^ @A@
    
    deriving Show


-- |
-- A sequent with antecedent @C@ and judgement @J@ claims that @J@ is
-- typechecked with respect to constants in @C@.
--
data Sequent = Sequent
    { antecedent    :: Context
    , judgement     :: Judgement
    }
    deriving Show


-- |
-- Giskard can reason about the things it knows through deductions.
-- The deductive step claims that if we can match some hypotheses,
-- then we can deduce a new idea as a conclusion.
-- 
-- These deductions can be simple typechecking rules, but also more
-- complex theorems that Giskard might be made aware of.
--
data Deduction = Deduction
    { hypotheses :: [Sequent]
    , conclusion :: Sequent
    }
    deriving Show

