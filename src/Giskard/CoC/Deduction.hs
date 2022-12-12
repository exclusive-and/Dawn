
-----------------------------------------------------------
-- |
-- Module       : Giskard.CoC.Deduction
-- Description  : Judgement and Deduction for the Calculus of Constructions
-----------------------------------------------------------
module Giskard.CoC.Deduction
    ( Judgement' (..), Judgement
    , Concept' (..), Concept
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
-- If a context @C@ assumes all the constants found in a judgement @J@,
-- then @C@ gives rise to the concept @J@, written @C |- J@.
--
data Concept' ctxt aterm atype
    = Concept ctxt (Judgement' aterm atype)
    deriving Show

-- NOTE:
-- Whether a judgement should or shouldn't include context seems
-- ambiguous. For now we separate judgement and concept so working
-- with plain judgements is easier.

-- |
-- Get the context of a concept.
--
context :: Concept' ctxt aterm atype -> ctxt
context (Concept ctxt _) = ctxt

-- |
-- Get the judgement of a concept.
--
judgement :: Concept' ctxt aterm atype -> Judgement' aterm atype
judgement (Concept _ j) = j


type Concept = Concept' Context Term Type


-- |
--
--
data Deduction' ctxt aterm atype
    = Deduction
        [Concept' ctxt aterm atype]
        (Concept' ctxt aterm atype)
    deriving Show

-- |
-- The premises of a deduction step.
hypotheses
    :: Deduction' ctxt aterm atype
    -> [Concept' ctxt aterm atype]

hypotheses (Deduction hyps _) = hyps

-- |
-- The conclusion of a deduction step.
--
conclusion
    :: Deduction' ctxt aterm atype
    -> Concept' ctxt aterm atype

conclusion (Deduction _ concl) = concl

type Deduction = Deduction' Context Term Type

