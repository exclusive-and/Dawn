
-----------------------------------------------------------
-- |
-- Module       : Giskard.CoC.Ppr
-- Description  : Pretty-printing for CoC Types
-----------------------------------------------------------
module Giskard.CoC.Ppr where

import              Giskard.CoC.Contexts
import              Giskard.CoC.Deduction
import              Giskard.CoC.Term
import              Giskard.CoC.Typechecking

import              Data.Text (Text)
import qualified    Data.Text as Text


pprTerm :: Term -> Text
pprTerm (T tm) = case tm of
    Star     -> "*"
    Var name -> name

    Pi bndr dom cod
        -> "(" <> bndr <> " : " <> pprTerm dom <> ")"
        <> " -> " <> pprTerm cod

    Lam bndr dom e
        -> "\\ (" <> bndr <> " : " <> pprTerm dom <> ")"
        <> " -> " <> pprTerm e

    Let bndr ty e body
        -> "let " <> bndr <> " : " <> pprTerm ty
        <> " := " <> pprTerm e
        <> " in " <> pprTerm body

    App f x
        -> "(" <> pprTerm f <> ") (" <> pprTerm x <> ")"


pprContext :: Context -> Text
pprContext = \case
    NamedContext ctxtName -> ctxtName

    ContextList xs -> Text.intercalate ", " $ map go xs where
        go (Assume name   ty) = "(" <> name <> " : " <> pprTerm ty <> ")"
        go (Define name e ty) =
            "(" <> name <> " : " <> pprTerm ty <> " := " <> pprTerm e <> ")"

    ConcatContexts ctxt1 ctxt2
        -> pprContext ctxt1 <> " ; " <> pprContext ctxt2


pprJudgement :: Judgement -> Text
pprJudgement = \case
    JIsAType ty -> "|- " <> pprTerm ty <> " Type"
    
    JTypeEquality ty1 ty2
        -> "|- " <> pprTerm ty1 <> " = " <> pprTerm ty2
    JTyping tm ty
        -> "|- " <> pprTerm tm <> " : " <> pprTerm ty
    JEquality tm1 tm2 ty
        -> "|- " <> pprTerm tm1 <> " = " <> pprTerm tm2 <> " : " <> pprTerm ty


pprSequent :: Sequent -> Text
pprSequent (Sequent ctxt j) =
    pprContext ctxt <> " " <> pprJudgement j


pprDeduction :: Deduction -> Text
pprDeduction (Deduction hyps concl) =
  let
    hyps' = map ((<> "\n") . pprConcept) hyps
    sep   = "------------------------------\n"
    concl' = pprConcept concl
  in
    Text.concat $ hyps' ++ [sep] ++ [concl']
