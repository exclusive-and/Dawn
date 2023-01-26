
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------
-- |
-- Module       : Giskard.Typechecking.Tc
-- Description  : Some Reusable Type-checker Types and Classes
-----------------------------------------------------------
module Giskard.Typechecking.Tc
    ( TcTerm, TcType
    , TcPoint (..)
    , TcMonad (..)
    , Typechecker (..), TcGoal (..)
    , TcHasTrace (..)
    , TcException (..)
    , TcHasMetaVars (..)
    , MetaVar
    , MetaVarMap, MetaVarRef (..)
    , newFlexiMetaVar
    , TcHasContext (..)
    , ContextVar, Context
    ) where

import              Giskard.Calculus
import              Giskard.Names
import              Giskard.Pretty

import qualified    Data.Kind as Kind
import              Data.Map (Map)
import qualified    Data.Map as Map
import              Data.Text (Text)


type TcTerm theTc = ProtoTerm (TcPoint theTc)

type TcType theTc = ProtoType (TcPoint theTc)

-- |
-- What kind of point should a typechecker use?
-- 
-- Examples:
--  * Simply-Typed Lambda Calculus: @TcPoint STLC = Name@.
--  * System F: @TcPoint SystemF = P@
-- 
data family TcPoint theTc :: Kind.Type

-- |
-- Which monad implements a typechecker?
--
data family TcMonad theTc :: Kind.Type -> Kind.Type


-- |
-- Type indices that are typecheckers.
-- 
class Monad (TcMonad theTc) => Typechecker theTc where
    -- |
    -- Check the actual type of a term against an expected type.
    -- Returns the term, since there may be specializations and
    -- reductions applied.
    check
        :: TcTerm theTc
        -> TcGoal theTc
        -> TcMonad theTc (TcTerm theTc)

    -- | Infer the type of a term.
    infer
        :: TcTerm theTc
        -> TcMonad theTc (TcTerm theTc, TcType theTc)

-- |
-- The typechecking goal: whether we're trying to check against
-- some type, or we're trying to infer the type of the term.
--
data TcGoal theTc
    -- | We want to check the type of this term against this type.
    = Check (TcType theTc)

    -- | We want to infer the type of this term.
    | Infer MetaVar


-- |
-- Typecheckers that have trace and exception reporting.
-- 
class Pretty (TcPoint theTc) => TcHasTrace theTc where
    -- | Trace TC execution with a helpful message.
    traceTC :: String -> Text -> TcMonad theTc ()
    
    -- | Throw a fatal typechecking exception.
    throwTC :: TcException theTc -> TcMonad theTc b

data TcException theTc
    -- | Mismatch in types: expected one type, got another.
    = Mismatch      (TcType theTc) (TcType theTc)
    -- | Couldn't find a point in the TC context.
    | NotInScope    (TcPoint theTc)
    -- | Generic failure with an explanatory message.
    | OtherTcErr    Text

instance Pretty (TcPoint theTc) => Pretty (TcException theTc) where
    pretty ex = case ex of
        Mismatch ty1 ty2
            -> "Mismatch between " <> pretty ty1 <> " and " <> pretty ty2
        NotInScope x
            -> "Variable " <> pretty x <> " not in scope"
        OtherTcErr err
            -> err


-- |
-- Typecheckers that have meta-variable solvers.
-- 
class Monad (TcMonad theTc) => TcHasMetaVars theTc where
    -- | Update the typechecker's meta-variable map.
    modifyMetaVars
        :: (MetaVarMap theTc -> MetaVarMap theTc)
        -> TcMonad theTc ()
    
    -- | Create a new meta-variable.
    newMetaVar :: TcMonad theTc MetaVar
    
    -- | Make a point from a meta-variable.
    mkMetaVarPoint :: MetaVar -> TcPoint theTc
    
    -- | Try to get the meta-variable from a point.
    maybeGetMetaVar :: TcPoint theTc -> Maybe MetaVar
    
    -- | Write to a meta-variable in the map.
    writeMetaVar
        :: MetaVar
        -> TcTerm theTc
        -> TcMonad theTc ()
    
    -- | Lookup a meta-variable in the map.
    readMetaVar
        :: MetaVar
        -> TcMonad theTc (MetaVarRef theTc)

type MetaVar = Name

type MetaVarMap theTc = Map MetaVar (MetaVarRef theTc)

data MetaVarRef theTc
    -- | Unfilled meta-var: can become anything.
    = Flexible
    -- | Filled-in meta-var.
    | Filled    (TcTerm theTc)

-- |
-- Create a new meta-variable pointing to a flexible reference.
-- 
newFlexiMetaVar
    :: (TcHasMetaVars theTc, TcHasContext theTc)
    => TcMonad theTc MetaVar
    
newFlexiMetaVar = do
    mv <- newMetaVar
    modifyMetaVars $ Map.insert mv Flexible
    -- modifyContext  $ Map.insert mv ty
    pure mv
{-
-- |
-- Create a new meta-variable and a type that is also a meta-variable.
-- 
newOpenMetaVar
    :: (TcHasMetaVars theTc, TcHasContext theTc)
    => TcMonad theTc MetaVar

newOpenMetaVar = do
-}


-- |
-- Typecheckers that have term typing contexts.
-- 
class Monad (TcMonad theTc) => TcHasContext theTc where
    -- | Update the typechecker's typing context.
    modifyContext
        :: (Context theTc -> Context theTc)
        -> TcMonad theTc ()
    
    -- | Make a point from a variable.
    mkVarPoint :: ContextVar -> TcPoint theTc
    
    -- | Try to get a variable from a point.
    maybeGetVar :: TcPoint theTc -> Maybe ContextVar
    
    -- |
    -- Create a new variable in the typing context, and pass it to
    -- a continuation.
    extendContext
        :: TcType theTc
        -> (TcPoint theTc -> TcMonad theTc (TcTerm theTc))
        -> TcMonad theTc (TcTerm theTc)
    
    -- | Get the type of a variable from the typing context.
    getVarType
        :: TcPoint theTc
        -> TcMonad theTc (TcType theTc)

type ContextVar = Name

type Context theTc = Map ContextVar (TcType theTc)
