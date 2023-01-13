
-----------------------------------------------------------
-- |
-- Module       : Giskard.Pretty.Term
-- Description  : Pretty-printing for Calculus Terms
-----------------------------------------------------------
module Giskard.Pretty.Term where

import              Giskard.Calculus.Term
import              Giskard.Pretty

import              Data.Text (Text, pack)
import qualified    Data.Text as Text


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

pprLet :: Int -> Type' Text -> Abs () Term' Text -> Term' Text -> Text
pprLet bindLvl dom e u =
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
    Let ty body e   -> parens $ pprLet bindLvl ty body e
    App f xs        -> parens $ pprApp bindLvl f xs
  where
    parens s = if shouldParen then "(" <> s <> ")" else s

pprTerm :: Ppr a => Term' a -> Text
pprTerm = pprTerm' False 0 . (pure . ppr =<<)

instance Ppr a => Ppr (Term' a) where ppr = pprTerm
        
