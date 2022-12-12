
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------
-- |
-- Module       : Giskard.CoC.Typechecking
-- Description  : Type Checker using Typing Judgements and Deductions
-----------------------------------------------------------
module Giskard.CoC.Typechecking
    where

import Giskard.CoC.Contexts
import Giskard.CoC.Deduction
import Giskard.CoC.Term


-- |
-- Trivial rule stating the existence of Star.
--
starRule :: Deduction
starRule = Deduction [] (Concept emptyCtxt (JTypeFormation mkStar))

-- |
-- Deductive statement of the lambda-abstraction rule.
--
abstractionRule :: Deduction
abstractionRule =
  let
    bndrs@[abndr, bbndr, ebndr, xbndr] = ["A", "B", "e", "x"]
    [a, b, e, x] = map mkVar bndrs

    ctxt0 = ConcatContexts (NamedContext "C") (ContextList [Assume xbndr a])
    hyp   = JTyping e b

    ctxt1 = NamedContext "C"
    concl = JTyping (mkLam xbndr a e) (mkPi xbndr a b)
  in
    Deduction [Concept ctxt0 hyp] (Concept ctxt1 concl)

-- |
-- Quick version of the abstraction rule that doesn't require unification
-- to instantiate terms.
--
quickAbstraction
    :: Context      -- ^ Context of the deduction.
    -> Name         -- ^ Binder to abstract over.
    -> Type         -- ^ Type of the abstracted variable.
    -> Term         -- ^ Term to abstract.
    -> Type         -- ^ Type of the term to abstract.
    -> Deduction
quickAbstraction ctxt bndr dom term ty =
  let
    ctxt0 = ConcatContexts ctxt (ContextList [Assume bndr dom])

    hyp   = JTyping term ty
    concl = JTyping (mkLam bndr dom term) (mkPi bndr dom ty)
  in
    Deduction [Concept ctxt0 hyp] (Concept ctxt concl)
