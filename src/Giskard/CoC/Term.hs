
-----------------------------------------------------------
-- |
-- Module       : Giskard.CoC.Term
-- Description  : Calculus of Constructions Term Syntax
-----------------------------------------------------------
module Giskard.CoC.Term
    ( Term (..), Type
    , Name
    ) where

import Data.List (zipWith)
import Data.Text (Text)


data Term
    = Var   Name            -- ^ Named free variable.
    | Rel   Int             -- ^ De Bruijn indexed variable - free or bound.
    | Pi    Type Type       -- ^ Pi-type binder @(x : A) -> B x@.
    | Lam   Type Term       -- ^ Lambda binder @\ (x : A) -> e@.
    | Let   Type Term Term  -- ^ Let-in binder @let x : A = e in b@.
    | App   Term [Term]     -- ^ Apply a term to a spine.
    | Star                  -- ^ Top-level type of telescopes.
    deriving Show


type Type = Term    

-- TEMPORARY
type Name = Text

