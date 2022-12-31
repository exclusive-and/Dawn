
-----------------------------------------------------------
-- |
-- Module       : Giskard.Calculus.Ppr
-- Description  : Pretty-printing for Calculus IR
-----------------------------------------------------------
module Giskard.Calculus.Ppr where

import              Giskard.Calculus.Contexts
import              Giskard.Calculus.Deduction
import              Giskard.Calculus.Term
import              Giskard.Names

import              Data.Text (Text, pack)
import qualified    Data.Text as Text


class Ppr a where
    ppr :: a -> Text

pprBound :: Int -> Text
pprBound boundId = "_bound_" <> pack (show boundId)

pprAbs :: Int -> Abs () Term' Text -> Text
pprAbs bindLvl tm =
    pprTerm' False (bindLvl + 1) $ instantiate1 (pure $ pprBound bindLvl) tm

pprPi :: Int -> Type' Text -> Abs () Type' Text -> Text
pprPi bindLvl dom cod =
  let
    x'   = pprBound bindLvl
    dom' = pprTerm' False bindLvl dom
    cod' = pprAbs bindLvl cod
   in
    "Pi (" <> x' <> " : " <> dom' <> ") -> " <> cod'

pprLam :: Int -> Type' Text -> Abs () Term' Text -> Text
pprLam bindLvl dom e =
  let
    x'   = pprBound bindLvl
    dom' = pprTerm' False bindLvl dom
    e'   = pprAbs bindLvl e
  in
    "Lam (" <> x' <> " : " <> dom' <> ") -> " <> e'

pprLet :: Int -> Type' Text -> Term' Text -> Abs () Term' Text -> Text
pprLet bindLvl dom u e =
  let
    x'   = pprBound bindLvl
    dom' = pprTerm' False bindLvl dom
    u'   = pprTerm' False bindLvl u
    e'   = pprAbs bindLvl e
  in
    "Let (" <> x' <> " : " <> dom' <> ") := " <> u' <> " In " <> e' 

pprApp :: Int -> Term' Text -> [Term' Text] -> Text
pprApp bindLvl f xs =
  let
    f'  = pprTerm' True bindLvl f
    xs' = map (pprTerm' True bindLvl) xs
  in
    f' <> " " <> Text.intercalate " " xs'
    
pprTerm' :: Bool -> Int -> Term' Text -> Text
pprTerm' shouldParen bindLvl tm = case tm of
    Star            -> "*"
    Point point     -> point
    Pi dom cod      -> parens $ pprPi bindLvl dom cod
    Lam dom e       -> parens $ pprLam bindLvl dom e
    Let ty e body   -> parens $ pprLet bindLvl ty e body
    App f xs        -> parens $ pprApp bindLvl f xs
  where
    parens s = if shouldParen then "(" <> s <> ")" else s

pprTerm :: Show a => Term' a -> Text
pprTerm = pprTerm' False 0 . (pure . pack . show =<<)

instance Show a => Ppr (Term' a) where ppr = pprTerm
        

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
    
