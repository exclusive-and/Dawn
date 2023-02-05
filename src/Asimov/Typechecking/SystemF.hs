
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------
-- |
-- Module       : Asimov.Typechecking.SystemF
-- Description  : A Simple System F Typechecker
-----------------------------------------------------------
module Asimov.Typechecking.SystemF where

import Asimov.Calculus
import Asimov.Names
import Asimov.Pretty
import Asimov.Typechecking.Tc
--import Asimov.Typechecking.Common

import Control.Lens (makeLenses, use, view, (%=), (.=))
import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Text (Text)


-- |
-- The System F typechecker type index.
-- 
data SystemF

-----------------------------------------------------------
-- Points
-----------------------------------------------------------

data instance TcPoint SystemF
    = Var  Name (Maybe Text)
    | Meta MetaVar
    deriving Eq

instance SynEq (TcPoint SystemF) where synEq = (==)

instance Pretty (TcPoint SystemF) where
    pretty p = case p of
        Var _ (Just nm) -> nm
        Var x Nothing   -> "_x_" <> pretty x
        Meta x          -> "_meta_" <> pretty x


-----------------------------------------------------------
-- Main Monad
-----------------------------------------------------------

data SystemFState = SystemFState
    { _traces     :: [(String, Text)]

    , _nameSupply :: Name

    , _termTyMap  :: Context SystemF
    , _metaVarMap :: MetaVarMap SystemF
    }

makeLenses ''SystemFState

newtype instance TcMonad SystemF a =
    SystemF
    { unSystemF :: ExceptT (TcException SystemF) (State SystemFState) a }
    deriving
        ( Functor, Applicative, Monad
        , MonadState SystemFState
        )


-----------------------------------------------------------
-- Monad Instances
-----------------------------------------------------------

-- Traces and Exceptions.

instance TcHasTrace SystemF where
    traceTC herald msg = traces %= ((herald, msg) :)

    throwTC = SystemF . throwE


-- Name Supply.

instance NameMonad (TcMonad SystemF) where
    newName = do
        i <- use nameSupply
        nameSupply .= i + 1
        pure i


-- Context Management.

instance TcHasContext SystemF where
    modifyContext f = termTyMap %= f
    
    mkVarPoint x = Var x Nothing
    
    maybeGetVar p = case p of
        Var x _ -> Just x
        Meta x  -> Just x

    extendContext ty cont = do
        x <- newName
        modifyContext $ Map.insert x ty
        cont $ Var x Nothing
    
    getVarType p
        | Just x <- maybeGetVar p
        = do
            ctxt <- use termTyMap
            case Map.lookup x ctxt of
                Just ty -> pure ty
                Nothing -> throwTC $ NotInScope p
        | otherwise
        = throwTC $ NotInScope p


-- Meta-variable Storage.

instance TcHasMetaVars SystemF where
    modifyMetaVars f = metaVarMap %= f

    newMetaVar = newName
    
    mkMetaVarPoint = Meta
    
    maybeGetMetaVar p =
        case p of
            Meta x -> Just x
            _      -> Nothing

    writeMetaVar mv tm =
        -- Just write, don't ask any questions.
        modifyMetaVars $ Map.insert mv (Filled tm)

    readMetaVar mv = do
        mvs <- use metaVarMap
        case Map.lookup mv mvs of
            Just mvref -> pure mvref
            -- Nothing    -> throwTC $ NotInMetaScope mv
            Nothing    -> throwTC $ OtherTcErr $ "mv not in scope: " <> pretty mv


-- Type Inference and Checking.

instance Typechecker SystemF where
    infer = undefined --inferSystemF
    check = undefined --checkSystemF

{-
getPType :: Monad m => P -> SystemF' m Type

getPType p@(Var nm _) = do
    ctx <- getContext
    case Map.lookup nm ctx of
        Just ty -> pure ty
        Nothing -> throwTC $ NotInScope p


inferSystemF :: Monad m => Term -> SystemF' m Type
inferSystemF tm = case tm of
    Point x -> getPointType x

    -- Check that @Π (x : A) -> B x : ★@.
    Pi dom cod -> do
        extendContext dom $ \x -> do
            let cod' = instantiate1 x cod
            check cod' Star

    -- Check that @λ (x : A) -> e : Π A -> B@.
    Lam dom e -> do
        extendContext dom $ \x -> do
            let e' = instantiate1 x e
            ety <- infer e'
            -- Lambdas should not have dependent types in System F.
            let oneEq = errIfEq ("Type " <> pretty ety <> " abs over " <> x) x
            etyAllFree <- traverse oneEq ety
            Pi . Abs . pure <$> ety'

    Let{} -> throwTC $ OtherTCErr "Let currently unsupported"

    -- Check that @∀ (x : A) -> B x : ★@.
    Forall dom cod -> do
        extendContext dom $ \x -> do
            let cod' = instantiate1 x cod
            check cod' Star

    App f xs -> inferApps f xs

    Star -> throwTC $ OtherTCErr "Can't infer type of Star"

-- |
-- Check if two variables are equal by their IDs. If not, return
-- the second one unaltered.
--
errIfEq :: (TCTraceMonad m, Eq a) => Text -> a -> a -> m a
errIfEq err x y
    | x == y    = throwTC $ OtherTCErr err
    | otherwise = pure y
    -}



