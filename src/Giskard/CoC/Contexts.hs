
-----------------------------------------------------------
-- |
-- Module       : Giskard.CoC.Contexts
-- Description  : Contexts and Telescopes of the Calculus of Constructions
-----------------------------------------------------------
module Giskard.CoC.Contexts
    ( Context (..)
    , Declaration (..)
    , emptyCtxt
    , ctxtToTele
    ) where

import Giskard.CoC.Term


-- |
-- Contexts implemented using lists. These store assumptions, like telescopes,
-- but can also store local definitions.
--
data Context
    -- | A context with a generic name: @C@.
    = NamedContext Name
    -- | A context list: @(x : A), (y : B), ...@.
    | ContextList [Declaration]
    -- | Concatenated contexts: @C ; D@
    | ConcatContexts
        Context
        Context
    deriving Show

data Declaration
    = Assume Name Type
    | Define Name Term Type
    deriving Show

-- |
-- Initial empty context.
--
emptyCtxt :: Context
emptyCtxt = ContextList []

-- |
-- Convert a context list to a telescope term.
--
ctxtToTele :: Context -> Term
ctxtToTele = go mkStar where
    go tele = \case
        -- (C : *) -> *
        NamedContext name -> Pi name Star tele

        ContextList xs
            -> foldr goDecl tele $ reverse xs where
                goDecl (Assume name   ty) tele1 = Pi name ty tele1
                goDecl (Define name _ ty) tele1 = Pi name ty tele1

        ConcatContexts ctxt1 ctxt2
            -> go (go tele ctxt2) ctxt1
