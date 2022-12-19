
-----------------------------------------------------------
-- |
-- Module       : Giskard.CoC.Ppr
-- Description  : Pretty-printing for CoC Types
-----------------------------------------------------------
module Giskard.CoC.Ppr where

import              Giskard.CoC.Term

import              Data.Text (Text, pack)
import qualified    Data.Text as Text


class Ppr a where
    ppr :: Int -> a -> Text

pprTerm :: Ppr a => Int -> Term' a -> Text
pprTerm lvl tm = case tm of
    Star    -> "*"
    Point p -> ppr (lvl - 1) p

    Pi dom cod
        -> "Pi" <> " ( " <> pack (show lvl) <> " : " <> ppr lvl dom <> " )" <> " -> " <> ppr lvl cod

    Lam dom e
        -> "Lam" <> " ( " <> pack (show lvl) <> " : " <> ppr lvl dom <> " )" <> " -> " <> ppr lvl e

    Let ty e body
        -> "let " <> "()" <> " : " <> ppr lvl ty
        <> " := " <> ppr lvl e
        <> " in " <> ppr lvl body

    App f xs
        -> "(" <> ppr lvl f <> ")" <> Text.concat (map (\x -> " (" <> ppr lvl x <> ")") xs)

instance Ppr a => Ppr (Term' a) where ppr = pprTerm

instance Ppr a => Ppr (Abs () Term' a) where ppr lvl = ppr (lvl + 1) . unAbs
  
instance Ppr a => Ppr (Point () a) where
    ppr lvl (Bound _)   = pack $ show lvl
    ppr lvl (Subterm a) = ppr lvl a

instance Ppr Text where ppr _ a = a
    
{-
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
    -}
    
