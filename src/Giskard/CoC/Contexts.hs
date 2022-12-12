
-----------------------------------------------------------
-- |
-- Module       : Giskard.CoC.Contexts
-- Description  : Contexts and Telescopes of the Calculus of Constructions
-----------------------------------------------------------
module Giskard.CoC.Contexts
    ( Context' (..)
    , Declaration' (..)
    , emptyCtxt
    , Context
    , ctxtToTele
    ) where

import Giskard.CoC.Term


-- |
-- Contexts implemented using lists. These store assumptions, like telescopes,
-- but can also store local definitions.
--
data Context' name aterm atype
    -- | A context with a generic name: @C@.
    = NamedContext name
    -- | A context list: @(x : A), (y : B), ...@.
    | ContextList [Declaration' name aterm atype]
    -- | Concatenated contexts: @C ; D@
    | ConcatContexts
        (Context' name aterm atype)
        (Context' name aterm atype)
    deriving Show

data Declaration' name aterm atype
    = Assume name atype
    | Define name aterm atype
    deriving Show

-- |
-- Initial empty context.
--
emptyCtxt :: Context' name aterm atype
emptyCtxt = ContextList []


type Context = Context' Name Term Type

-- |
-- Convert a context list to a telescope term.
--
ctxtToTele :: Context -> Term
ctxtToTele = go mkStar where
    go tele = \case
        -- (C : *) -> *
        NamedContext name -> mkPi name mkStar tele

        ContextList xs
            -> foldr goDecl tele $ reverse xs where
                goDecl (Assume name   ty) tele1 = T (Pi name ty tele1)
                goDecl (Define name _ ty) tele1 = T (Pi name ty tele1)

        ConcatContexts ctxt1 ctxt2
            -> go (go tele ctxt2) ctxt1
