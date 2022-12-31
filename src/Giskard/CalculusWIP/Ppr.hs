
module Giskard.CalculusWIP.Ppr where

import Giskard.Calculus.Term
import Giskard.CalculusWIP.Deduction
import Giskard.CalculusWIP.Contexts


pprContext :: Context -> Text
pprContext = \case
    NamedContext ctxtName -> pack $ show ctxtName

    ContextList xs -> Text.intercalate ", " $ map go xs where
        go (Assume name   ty) = "(" <> pack (show name) <> " : " <> ppr ty <> ")"
        go (Define name e ty) =
            "(" <> pack (show name) <> " : " <> ppr ty <> " := " <> ppr e <> ")"

    ConcatContexts ctxt1 ctxt2
        -> pprContext ctxt1 <> " ; " <> pprContext ctxt2

instance Ppr Context where ppr = pprContext
        
        
pprJudgement :: Judgement -> Text
pprJudgement = \case
    JIsAType ty -> "|- " <> ppr ty <> " Type"
    
    JTypesAreEqual ty1 ty2
        -> "|- " <> ppr ty1 <> " = " <> ppr ty2
    JOfType tm ty
        -> "|- " <> ppr tm <> " : " <> ppr ty
    JAreEqual tm1 tm2 ty
        -> "|- " <> ppr tm1 <> " = " <> ppr tm2 <> " : " <> ppr ty

instance Ppr Judgement where ppr = pprJudgement

pprSequent :: Sequent -> Text
pprSequent (Sequent ctxt j) =
    pprContext ctxt <> " " <> pprJudgement j

instance Ppr Sequent where ppr = pprSequent

pprDeduction :: Deduction -> Text
pprDeduction (Deduction hyps concl) =
  let
    hyps' = map ((<> "\n") . pprSequent) hyps
    sep   = "------------------------------\n"
    concl' = pprSequent concl
  in
    Text.concat $ hyps' ++ [sep] ++ [concl']

instance Ppr Deduction where ppr = pprDeduction
    
