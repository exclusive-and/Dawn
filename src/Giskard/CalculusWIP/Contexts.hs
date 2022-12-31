
-----------------------------------------------------------
-- |
-- Module       : Giskard.Calculus.Contexts
-- Description  : Contexts and Telescopes of the Calculus
-----------------------------------------------------------
module Giskard.Calculus.Contexts
    ( Context (..)
    , emptyCtxt
    , Declaration (..)
    , declName, declType
    , ctxtToTele
    ) where

import Giskard.Calculus.Term
import Giskard.Names


-- |
-- Contexts implemented using lists. These store assumptions, like telescopes,
-- but can also store local definitions.
--
data Context
    -- | A context with a generic name: @C@.
    = NamedContext   Name
    -- | A context list: @(x : A), (y : B), ...@.
    | ContextList    [Declaration]
    -- | Concatenated contexts: @C ; D@
    | ConcatContexts Context Context

-- |
-- Initial empty context.
--
emptyCtxt :: Context
emptyCtxt = ContextList []
    
data Declaration
    = Assume Name Type
    | Define Name Term Type

declName :: Declaration -> Name
declName = \case
    Assume nm   _ -> nm
    Define nm _ _ -> nm

declType :: Declaration -> Type
declType = \case
    Assume _   ty -> ty
    Define _ _ ty -> ty

-- |
-- Convert a context list to a telescope term.
--
ctxtToTele :: Context -> Term
ctxtToTele = go Star where
    go tele = \case
        -- (C : *) -> *
        NamedContext name -> mkPi name Star tele

        ContextList xs
            -> foldr goDecl tele $ reverse xs where
                goDecl (Assume name   ty) tele1 = mkPi name ty tele1
                goDecl (Define name _ ty) tele1 = mkPi name ty tele1

        ConcatContexts ctxt1 ctxt2
            -> go (go tele ctxt2) ctxt1
