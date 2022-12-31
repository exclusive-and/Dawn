
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------
-- |
-- Module       : Giskard.Telepath.Types
-- Descrption   : Term Calculus Implementation of Telepath's Type System
-----------------------------------------------------------
module Giskard.Telepath.Types where

import              Giskard.Calculus.Term hiding (Term, Type)
import              Giskard.Calculus.Ppr
import              Giskard.Names

import              Control.Monad
import              Control.Monad.State.Lazy
import              Control.Monad.Trans.Except
import              Data.Functor.Identity
import              Data.Map (Map)
import qualified    Data.Map as Map
import              Data.Text (Text)
import qualified    Data.Text as Text
import              GHC.Int

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
    = IntLit        Int
    | MatIntLit     Int Int (Matrix Int64)
    | DoubleLit     Double
    | MatDoubleLit  Int Int (Matrix Double)
    deriving (Eq, Show)

-- |
-- Create a variable point.
--
var :: Name -> Term
var = Point . Var
    
-- |
-- Create an integer literal point.
--
intLit :: Int -> Term
intLit = Point . Lit . IntLit

-- |
-- Create a double literal point
--
doubleLit :: Double -> Term
doubleLit = Point . Lit . DoubleLit
    

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
        IntLit _            -> integer
        MatIntLit m n _     -> App matrix [intLit m, intLit n, integer]
        DoubleLit _         -> double
        MatDoubleLit m n _  -> App matrix [intLit m, intLit n, double]
        
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
    go ty remaining = throwTC $ OtherTCErr $ "Expected pi-type, got "
                                          <> ppr hd <> " : " <> ppr ty
    
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
integer  = var 1
double   = var 2
matrix   = var 3

integerTy, doubleTy, matrixTy :: Term
integerTy = star
doubleTy  = star
matrixTy  = mkPi (Var 10) integer
          $ mkPi (Var 11) integer
          $ mkPi (Var 12) star $ star

-- |
-- Matrix constructor type.
-- 
mkMatrixTy :: Term
mkMatrixTy = mkPi (Var 10) integer
           $ mkPi (Var 11) integer
           $ mkPi (Var 12) star
           $ App matrix [var 10, var 11, var 12]

-- |
-- Matrix constructor.
-- 
mkMatrix :: Term
mkMatrix = var 4

-- |
-- Type of matrix multiplication: 
-- (m k n : Nat) -> (t : Type) -> Mat m k t -> Mat k n t -> Mat m n t
matmulTy :: Type
matmulTy =
  let
    -- a := Mat m k t
    aty = App matrix [var 10, var 11, var 13]
    -- b := Mat k n t
    bty = App matrix [var 11, var 12, var 13]
    -- c := Mat m n t
    cty = App matrix [var 10, var 12, var 13]
    
    -- h := Mat m k t -> Mat k n t -> Mat m n t
    hty = mkPi (Var 14) aty $ mkPi (Var 15) bty $ cty
    
    -- g := (t : Type) -> h
    gty = mkPi (Var 13) star hty
    
    -- f := (m k n : Nat) -> g
    --   == (m k n : Nat) -> Mat m k t -> Mat k n t -> Mat m n t
    fty = mkPi (Var 10) integer
        $ mkPi (Var 11) integer
        $ mkPi (Var 12) integer $ gty
  in
    fty

-- |
-- Matrix multiplication.
-- 
matmul :: Term
matmul = var 5
    

-- |
-- Important constants:
--
--  (0) Type
--  (1) Int      : Type
--  (2) Double   : Type
--  (3) Matrix   : Int -> Int -> Type -> Type
--  (4) MkMatrix : (m n : Int) -> (t : Type) -> Matrix m n t
--  (5) Matmul   : (m k n : Int) -> (t : Type)
--              -> Matrix m k t -> Matrix k n t -> Matrix m n t
-- 
constants :: Map Name Term
constants = Map.fromList
    [ (1, integerTy )
    , (2, doubleTy  )
    , (3, matrixTy  )
    , (4, mkMatrixTy)
    , (5, matmulTy  )
    ]
    
