
-----------------------------------------------------------
-- |
-- Module       : Giskard.Calculus.Ppr
-- Description  : Pretty-printing for Calculus IR
-----------------------------------------------------------
module Giskard.Calculus.Ppr where

import              Giskard.Calculus.Term

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
        
