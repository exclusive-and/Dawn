
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------
-- |
-- Module       : Giskard.CoC.Typechecking
-- Description  : Type Checker using Typing Judgements and Deductions
-----------------------------------------------------------
module Giskard.Typechecking.CoC
    where

import Giskard.Calculus.Contexts
import Giskard.Calculus.Deduction
import Giskard.Calculus.Term


-- |
-- Trivial rule stating the existence of Star.
--
starRule :: Deduction
starRule = Deduction [] (Sequent emptyCtxt (JIsAType Star))

-- |
-- Deductive statement of the lambda-abstraction rule.
--
abstractionRule :: Deduction
abstractionRule =
  let
    bndrs@[abndr, bbndr, ebndr, xbndr] = ["A", "B", "e", "x"]
    [a, b, e, x] = map Point bndrs

    ctxt0 = ConcatContexts (NamedContext "C") (ContextList [Assume xbndr a])
    hyp   = JOfType e b

    ctxt1 = NamedContext "C"
    concl = JOfType (mkLam xbndr a e) (mkPi xbndr a b)
  in
    Deduction [Sequent ctxt0 hyp] (Sequent ctxt1 concl)

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

    hyp   = JOfType term ty
    concl = JOfType (mkLam bndr dom term) (mkPi bndr dom ty)
  in
    Deduction [Sequent ctxt0 hyp] (Sequent ctxt concl)
