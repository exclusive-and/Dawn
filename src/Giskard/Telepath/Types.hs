
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------
-- |
-- Module       : Giskard.Telepath.Types
-- Descrption   : Term Calculus Implementation of Telepath's Type System
-----------------------------------------------------------
module Giskard.Telepath.Types where

import              Giskard.Calculus.Term hiding (Term, Type)
import              Giskard.Names

import              Control.Monad
import              Control.Monad.State.Lazy
import              Control.Monad.Trans.Except
import              Data.Functor.Identity
import              Data.Map (Map)
import qualified    Data.Map as Map
import              Data.Text (Text)

import              Numeric.LinearAlgebra (Matrix)
import qualified    Numeric.LinearAlgebra as M


-----------------------------------------------------------
-- Typechecker
-----------------------------------------------------------

type Term = Term' P
type Type = Term

-- |
-- Points can be variable names or literals.
-- 
data P = Var Name | Lit Literal
    deriving (Eq, Show)

instance SynEq P where synEq = (==)

data Literal
    = IntLit    Int
    | DoubleLit Double
    deriving (Eq, Show)


data TCException
    = Mismatch      Type Type   -- ^ Expected one type, got another.
    | NotInScope    Name        -- ^ Couldn't find a variable in the context.
    | OtherTCErr    Text        -- ^ Generic exception with message.

data TCState = TCState
    { termTyMap     :: Map Name Term
    , nameSupply    :: Name
    }

-- |
-- Main typechecker monad transformer. Stores the context and environment
-- being typechecked.
-- 
newtype TCMT m a = TCM
    { runTC :: ExceptT TCException (StateT TCState m) a }
    deriving ( Functor, Applicative, Monad
             , MonadState TCState )

-- |
-- Throw a typechecking exception.
-- 
throwTC :: Monad m => TCException -> TCMT m a
throwTC = TCM . throwE
    
instance Monad m => NameMonad (TCMT m) where
    newName = do
        i <- nameSupply <$> get
        modify $ \ s -> s { nameSupply = i + 1 }
        pure i

type TCM = TCMT Identity

-- |
-- Extend a context by tracking a free variable's type.
-- 
extendContext :: Monad m => Name -> Type -> TCMT m ()
extendContext x ty = do
    tyMap <- termTyMap <$> get
    let ext = modify $ \ s -> s { termTyMap = Map.insert x ty tyMap }
    case Map.lookup x tyMap of
        Nothing -> ext
        Just oldTy
            | synEq ty oldTy -> pure () -- Do nothing
            | otherwise      -> throwTC $ Mismatch ty oldTy

-- |
-- Get the type of a free variable from the context.
-- 
getVarType :: Monad m => Name -> TCMT m Type
getVarType x = do
    tyMap <- termTyMap <$> get
    case Map.lookup x tyMap of
        Just ty -> pure ty
        Nothing -> throwTC $ NotInScope x

-- |
-- Get the type of a literal.
-- 
getLitType :: Monad m => Literal -> TCMT m Type
getLitType x =
    pure $ case x of
        IntLit _    -> integer
        DoubleLit _ -> double
        
-- |
-- Infer the type of a term.
-- 
infer :: Monad m => Term -> TCMT m Type
infer tm = case tm of
    Point (Var x) -> getVarType x
    Point (Lit x) -> getLitType x
    
    -- Check that @Π (x : A) -> B x : ★@.
    Pi dom cod -> do
        x <- newName
        extendContext x dom
        let cod' = instantiate1 (Point $ Var x) cod
        -- The codomain should always evaluate to a type.
        check cod' Star
    
    -- Check that @λ (x : A) -> e : Π (x : A) -> B x@.
    Lam dom e -> do
        x <- newName
        extendContext x dom
        let e' = instantiate1 (Point $ Var x) e
        mkPi (Var x) dom <$> infer e'
    
    -- Check that @(f : A -> B) (x : A) : B@
    App f args -> inferApp f args
    
    Star -> throwTC $ OtherTCErr "Can't infer type of Star"

-- |
-- Infer the type of an application.
-- 
inferApp :: Monad m => Term -> [Term] -> TCMT m Type
inferApp hd args = do
    fty <- infer hd
    go fty args
  where
    go :: Monad m => Type -> [Term] -> TCMT m Type
    -- Is a pi-type and has arguments.
    go (Pi dom cod) (x:args) = do
        -- We don't actually care about the argument's type; just
        -- that it's compatible with the function's domain.
        _ <- check x dom
        
        let cod' = instantiate1 x cod
        go cod' args

    -- No more arguments to check.
    go ty [] = pure ty
    
    -- Not a pi-type, but there are still some arguments left.
    go ty _ = throwTC $ OtherTCErr "Expected pi-type"
    
-- |
-- Typecheck a term against an expected type.
--
check :: Monad m => Term -> Type -> TCMT m Type
check tm expected = do
    ty <- infer tm
    if synEq expected ty
       then pure ty
       else throwTC $ Mismatch ty expected


-----------------------------------------------------------
-- Builtins
-----------------------------------------------------------
       
star, integer, double, matrix :: Term
star     = Star
integer  = Point (Var 1)
double   = Point (Var 2)
matrix   = Point (Var 3)

integerTy, doubleTy, matrixTy :: Term
integerTy = star
doubleTy  = star
matrixTy  = mkPi (Var 4) integer
          $ mkPi (Var 5) integer
          $ mkPi (Var 6) star $ star
       
-- |
-- Important constants:
--
--  (0) Type
--  (1) Int     : Type
--  (2) Double  : Type
--  (3) Matrix  : Int -> Int -> Type -> Type
-- 
constants :: Map Name Term
constants = Map.fromList
    [ (1, integerTy)
    , (2, doubleTy )
    , (3, matrixTy )
    ]
    
{-
-- |
-- (m k n : Nat) -> (t : Type) -> Mat m k t -> Mat k n t -> Mat m n t
matmulTy :: Type
matmulTy =
  let
    -- a := Mat m k t
    aty = App matrix [Point 7, Point 5, Point 4]
    -- b := Mat k n t
    bty = App matrix [Point 7, Point 6, Point 5]
    -- c := Mat m n t
    cty = App matrix [Point 7, Point 6, Point 4]
    
    -- g := (t : Type) -> Mat m k t -> Mat k n t -> Mat m n t
    gty = mkPi 7 star $ mkPi 8 aty $ mkPi 9 bty $ cty
    
    -- f := (m k n : Nat) -> g
    --   == (m k n : Nat) -> Mat m k t -> Mat k n t -> Mat m n t
    fty = mkPi 4 integer $ mkPi 5 integer $ mkPi 6 integer gty
  in
    fty
    -}
