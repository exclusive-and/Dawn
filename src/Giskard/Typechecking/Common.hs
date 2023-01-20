
-----------------------------------------------------------
-- |
-- Module       : Giskard.Typechecking.Common
-- Description  : Common Typechecking Functionality
-----------------------------------------------------------
module Giskard.Typechecking.Common where

import Giskard.Calculus.ProtoTerm
import Giskard.Calculus.SyntacticEq
import Giskard.Names
import Giskard.Pretty
import Giskard.Typechecking.Tc

import Control.Monad (zipWithM_)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text (pack)


-- |
-- Tries to unify terms by filling in meta-variables.
-- 
unifyMetaVars
    :: ( SynEq (TcPoint theTc)
       , TcHasTrace theTc
       , TcHasMetaVars theTc
       )
    => TcTerm theTc
    -> TcTerm theTc
    -> TcMonad theTc ()

unifyMetaVars = go Set.empty where

    go :: ( SynEq (TcPoint theTc)
          , TcHasTrace theTc
          , TcHasMetaVars theTc
          )
       => Set MetaVar
       -- ^ Fictitious points created by the unifier that should not be
       -- filled in; only checked for syntactic equality.
       -> TcTerm theTc
       -> TcTerm theTc
       -> TcMonad theTc ()
    
    -- If either term is a fictitious point, then anything other than
    -- syntactic equality is a unification failure. If either term is a
    -- point that is /not/ fictitious, try to do normal point unification.

    go fictitious tm1@(Point p) tm2
        | Just mv <- maybeGetMetaVar p
        , mv `Set.member` fictitious = goFictitious tm1 tm2
        | otherwise                  = goPoint fictitious p tm2

    go fictitious tm1 tm2@(Point p)
        | Just mv <- maybeGetMetaVar p
        , mv `Set.member` fictitious = goFictitious tm1 tm2
        | otherwise                  = goPoint fictitious p tm1

    -- If the terms are abstractions, instantiate the abstraction with a
    -- fictitious point and continue.

    go fictitious (Lam dom1 e1) (Lam dom2 e2) = do
        go fictitious dom1 dom2

        x <- newMetaVar
        let fict' = Set.insert x fictitious
            var   = Point $ mkMetaVarPoint x
            e1'   = instantiate1 var e1
            e2'   = instantiate1 var e2
        go fict' e1' e2'

    go fictitious (Pi dom1 cod1) (Pi dom2 cod2) = do
        go fictitious dom1 dom2

        x <- newMetaVar
        let fict' = Set.insert x fictitious
            var   = Point $ mkMetaVarPoint x
            cod1' = instantiate1 var cod1
            cod2' = instantiate1 var cod2
        go fict' cod1' cod2'

    go fictitious (Forall dom1 cod1) (Forall dom2 cod2) = do
        go fictitious dom1 dom2

        x <- newMetaVar
        let fict' = Set.insert x fictitious
            var   = Point $ mkMetaVarPoint x
            cod1' = instantiate1 var cod1
            cod2' = instantiate1 var cod2
        go fict' cod1' cod2'

    -- If the terms are both applications, just unify them piece-wise.

    go fictitious (App f1 xs1) (App f2 xs2) = do
        go fictitious f1 f2
        zipWithM_ (go fictitious) xs1 xs2

    go _ Star Star = pure ()
        
    -- TODO: actually emit constraints rather than failing.
    go _ tm1 tm2 = throwTC $ OtherTcErr $
        "Meta-variable unification failed: " <> ppr tm1 <> ", " <> ppr tm2

    -- Check that fictitious points are indeed equal.

    goFictitious
        :: ( SynEq (TcPoint theTc)
           , TcHasTrace theTc
           , TcHasMetaVars theTc
           )
        => TcTerm theTc
        -> TcTerm theTc
        -> TcMonad theTc ()

    goFictitious tm1 tm2
        | synEq tm1 tm2 = pure ()
        | otherwise
        = throwTC $ OtherTcErr
            $ "Couldn't match fictitious binders "
           <> ppr tm1 <> " and " <> ppr tm2

    -- Try to unify points. If the point is a meta-variable, try to fill
    -- it in. Otherwise, make sure that they're syntactically equal.

    goPoint
        :: ( SynEq (TcPoint theTc)
           , TcHasTrace theTc
           , TcHasMetaVars theTc
           )
        => Set MetaVar
        -> TcPoint theTc
        -> TcTerm theTc
        -> TcMonad theTc ()

    goPoint fictitious p tm2
        | Just mv <- maybeGetMetaVar p
        = do
            traceTC "goPoint" $ ppr mv <> " := " <> ppr tm2
            res <- readMetaVar mv
            case res of
                Filled tm1 -> go fictitious tm1 tm2
                Flexible   -> unifyUnfilledMetaVar mv tm2

    goPoint _ p1 (Point p2)
        | synEq p1 p2 = pure ()

    -- TODO: actually emit constraints rather than failing.
    goPoint _ tm1 tm2 = throwTC $ OtherTcErr $
        "Meta-variable unification failed: " <> ppr tm1 <> ", " <> ppr tm2
    
-- |
-- Unify a meta-variable by filling it in.
-- 
unifyUnfilledMetaVar
    :: TcHasMetaVars theTc
    => MetaVar
    -> TcTerm theTc
    -> TcMonad theTc ()
    
unifyUnfilledMetaVar = writeMetaVar
    
{-
inferAppInst :: (Ord a, TCMonad a m) => Type' a -> [Term' a] -> m (Type' a)
inferAppInst funTy args = do
    go Set.empty [] [] funTy args

  where
    go, iall, iresult, ivar, iarg
        :: (Ord point, TCMonad point tcm)
        => Set point        -- ^ Set of generated type meta-variables.
        -> [Term' point]    -- ^ Accumulator of applied arguments.
        -> [Type' point]    -- ^ Arguments applied so far (for error info).
        -> Type' point      -- ^ Sigma-type being worked on.
        -> [Term' point]    -- ^ Arguments left to apply to the sigma.
        -> tcm
            ( Set point     -- ^ New set of generated meta-variables.
            , [Term' point] -- ^ New arguments applied.
            , Type' point   -- ^ New rho-type to work on.
            )

    go delta acc soFar funTy args = do

    -- Fill in any top-level 'Forall's with fresh instantiation variables.
    iall delta acc soFar funTy args
        | Forall{} <- funTy
        = do
            (kappas, workTy) <- metaFillTLForalls funTy
            let delta' = delta `Set.union` Set.fromList kappas
            iresult delta' acc soFar workTy args

    -- We're done; just return everything we've accumulated.
    iresult delta acc _ funTy [] = pure (delta, acc, funTy)

    -- Still work left to do; fall through to IVAR.
    iresult delta acc soFar funTy args =
        ivar delta acc soFar funTy args

    -- Function was of the form @f :: forall b. a -> b@, and IALL
    -- instantiated @b@ with @kappa@. Application is of the form @f x y@
    -- where @x :: a@ and @y :: mu@. So we must set @kappa := mu -> nu@
    -- to accept the argument @y@.
    ivar delta acc soFar funTy args@(_:_)
        | Point kappa <- funTy
        , kappa `Set.member` delta
        = do
            let nargs = length args
            argNus <- replicateM nargs newOpenMetaVar
            resNu  <- newOpenMetaVar

            let delta' = delta `Set.union` Set.fromList (resNu : argNus)
                funTy' =
                    foldr (\x y -> Pi x $ mkNoAbs y) resNu argNus

            writeMetaVar kappa funTy'
            go delta' acc soFar funTy' args

    -- Usual application case: unify the argument's type with the
    -- expected parameter's type and fill any meta-variables.
    iarg delta acc soFar funTy (arg : args) = do


-- |
-- Fill in all the top-level 'Forall' binders in a type with fresh
-- instantiation variables. We'll substitute these instantiation
-- variables for concrete types later.
--
metaFillTLForalls :: TCMonad a m => Type' a -> m ([a], Type' a)
metaFillTLForalls ty = go ty [] where
    -- Got a forall; create a tyvar and instantiate it into the codomain.
    go (Forall dom cod) tvs =
        x <- newFlexiMetaVar
        let cod' = instantiate1 x cod
        go cod' (x : tvs)

    -- We're out of foralls; return all the tyvars we generated.
    go ty tvs = pure (ty, tvs)
    -}
