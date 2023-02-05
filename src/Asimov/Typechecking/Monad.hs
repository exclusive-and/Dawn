
-----------------------------------------------------------
-- |
-- Module       : Giskard.Typechecking.Monad
-- Description  : Typechecking Monad and Ancillary Monads
-----------------------------------------------------------
module Giskard.Typechecking.Monad where

import Giskard.Calculus.Term
import Giskard.Calculus.Pretty.Term

import Giskard.Names
import Giskard.Pretty

import Control.Monad


-----------------------------------------------------------
-- Main Type Checking and Inference Monad
-----------------------------------------------------------

-- |
-- A monad that acts like a typechecker.
--
class  ( Monad m
       , TraceMonad point m
       , IsPoint point
       , ContextMonad point m
       , MetaVarMonad point m
       )
    => TCMonad point m where

    -- | Infer the type of a term within the TC monad.
    infer :: Term' point -> m (Type' point)

    -- | Check a term's inferred type against an expected type.
    check :: Term' point -> Type' point -> m (Type' point)

    -- | Get the type of a point from the TC context.
    getPointType :: point -> m (Type' point)

-- |
-- Something that we can use as a point in TC.
--
-- In practice this just means that it stores a 'Name' we can look up.
--
class IsPoint point where
    -- |
    -- Try to get the standard ID of a point.
    --
    -- Reports a 'TCException' if the point doesn't have an ID.
    getPointName :: TraceMonad point m => point -> m Name
    
    -- |
    -- Maybe try to get the name of a meta-variable. Returns
    -- @Just name@ if the name is a meta-variable, and @Nothing@
    -- otherwise.
    -- 
    -- Reports a 'TCException' if the point doesn't have an ID.
    maybeGetMetaVar :: TraceMonad point m => point -> m (Maybe Name)


-----------------------------------------------------------
-- Ancillary Monads and Types
-----------------------------------------------------------

-- Traces and Exceptions.

-- |
-- Monads with information reporting and TC exception handling.
--
class Monad m => TraceMonad point m where
    -- | Trace TC execution with a helpful message.
    traceTC :: String -> Text -> m b

    -- | Throw a fatal typechecking exception.
    throwTC :: TCException' point -> m b

data TCException' a
    -- | Mismatch in types: expected one type, got another.
    = Mismatch      (Type' a) (Type' a)
    -- | Couldn't find a point in the TC context.
    | NotInScope    a
    -- | Generic failure with an explanatory message.
    | OtherTCErr    Text

instance Ppr a => Ppr (TCException' a) where
    ppr (Mismatch ty1 ty2) =
        "Mismatch between " <> ppr ty1 <> " and " <> ppr ty2

    ppr (NotInScope x) = "Variable " <> ppr nm <> " not in scope"

    ppr (OtherTCErr err) = err


-- Context Management.

class (Monad m, TraceMonad point m, IsPoint point)
    => ContextMonad point m where
    -- | Get the current TC context.
    getContext :: m (Context' point)

    setContext :: Context' point -> m ()

    -- | Add the typing of a variable to the context.
    addToContext :: point -> Type' point -> m ()

    -- | Extend the TC context with a point to execute a continuation.
    extendContext
        :: Type' point -> (point -> m (Term' point))
        -> m (Term' point)

type Context' a = Map Name (Type' a)


-- Meta-variable Storage.

-- |
-- A monad that implements meta-variable reading and writing.
--
class (Monad m, TraceMonad point m, IsPoint point)
    => MetaVarMonad point m where
    -- | Get the meta-variable map from the monad.
    getMetaVars :: m (MetaVars' point)

    -- | Create a new meta-variable.
    newMetaVar :: m point

    -- | Try to write to a meta-variable; filling it in.
    writeMetaVar :: Name -> Term' point -> m ()

    -- | Try to read from a meta-variable.
    readMetaVar :: Name -> m (MetaVarRef' point)

data MetaVarRef' a
    -- | Unfilled meta-var: can become anything.
    = Flexible
    -- | Filled-in meta-var.
    | Filled    (Term' a)

type MetaVars' a = Map Name (MetaVarRef a)



-- TODO: Constraint generation and solving.

-- |
-- Constraints the typechecker generates for the constraint solver.
--
-- Notes about constraint solving:
--
--
--
data Constraint' a
    -- | A var must be equal to some term.
    = CtEq a (Term' a)

instance Ppr a => Constraint' a where
    ppr (CtEq a tm) = "Constraint: " <> ppr a <> " ~ " <> ppr tm


-- TODO: Checkpointing.

-- |
-- Checkpoints keep track of the changes to the context that happen
-- as we generate new variables to go under binders.
--
-- Notes about checkpointing:
--
--
--
type Checkpoints' a = Map CheckpointId (Subst' a)

type CheckpointId = Name
