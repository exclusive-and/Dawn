
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

import              Control.Monad.State.Lazy
import              Control.Monad.Trans.Except
import              Data.Functor.Identity
import              Data.Map (Map)
import qualified    Data.Map as Map
import              Data.Text (Text, pack, unpack)
import              GHC.Int

import              Numeric.LinearAlgebra (Matrix)


-----------------------------------------------------------
-- Typechecker
-----------------------------------------------------------

type Term = Term' P
type Type = Term

-- |
-- Points can be variable names or literals.
-- 
data P = Var Name (Maybe Text) | Lit Literal

instance Eq P where
    Var nm1 _ == Var nm2 _ = nm1 == nm2
    Lit lit1  == Lit lit2  = lit1 == lit2
    _         == _         = False

instance Show P where
    show (Var _ (Just nm)) = unpack nm
    show (Var nm _       ) = "_x_" ++ show nm
    show (Lit lit        ) = show lit

instance SynEq P where synEq = (==)

data Literal
    = IntLit        Int
    | MatIntLit     Int Int (Matrix Int64)
    | DoubleLit     Double
    | MatDoubleLit  Int Int (Matrix Double)
    deriving Eq
    
instance Show Literal where
    show (IntLit x          ) = show x
    show (MatIntLit _ _ x   ) = show x
    show (DoubleLit x       ) = show x
    show (MatDoubleLit _ _ x) = show x
    
-- |
-- Create a variable point.
--
var :: Name -> Term
var = Point . varP

-- |
-- Create a var P.
-- 
varP :: Name -> P
varP nm = Var nm Nothing

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

instance Show TCException where
    show (Mismatch ty1 ty2) =
        unpack $ "Mismatch between " <> ppr ty1 <> " and " <> ppr ty2
        
    show (NotInScope nm) =
        unpack $ "Variable " <> pack (show nm) <> " not in scope"
    
    show (OtherTCErr err) = unpack err
    
data TCState = TCState
    { termTyMap     :: Map Name Term
    , nameSupply    :: Name
    }

-- |
-- Main typechecker monad transformer. Stores the context and environment
-- being typechecked.
-- 
newtype TCMT m a = TCM
    { unTC :: ExceptT TCException (StateT TCState m) a }
    deriving ( Functor, Applicative, Monad
             , MonadState TCState )

instance MonadTrans TCMT where
    lift m = TCM . ExceptT $ StateT $ \ s -> do
        a <- m
        pure (Right a, s)

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
-- Run a typechecker monad transformer.
-- 
runTCMT :: Monad m => TCMT m a -> TCState -> m (Either TCException a, TCState)
runTCMT tcm = runStateT (runExceptT $ unTC tcm)

-- |
-- Run a typechecker monad.
--
runTC :: TCM a -> TCState -> (Either TCException a, TCState)
runTC tcm = runState (runExceptT $ unTC tcm)

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
        MatIntLit m n _     -> App matrixTyCon [intLit m, intLit n, integer]
        DoubleLit _         -> double
        MatDoubleLit m n _  -> App matrixTyCon [intLit m, intLit n, double]
        
-- |
-- Infer the type of a term.
-- 
infer :: Monad m => Term -> TCMT m Type
infer tm = case tm of
    Point (Var x _) -> getVarType x
    Point (Lit x  ) -> getLitType x
    
    -- Check that @Π (x : A) -> B x : ★@.
    Pi dom cod -> do
        x <- newName
        extendContext x dom
        let cod' = instantiate1 (var x) cod
        -- The codomain should always evaluate to a type.
        check cod' Star
    
    -- Check that @λ (x : A) -> e : Π (x : A) -> B x@.
    Lam dom e -> do
        x <- newName
        extendContext x dom
        let e' = instantiate1 (var x) e
        mkPi (varP x) dom <$> infer e'
    
    -- Check that @(f : A -> B) (x : A) : B@
    App f args -> inferApp f args
    
    Star -> throwTC $ OtherTCErr "Can't infer type of Star"
    
    Let{} -> throwTC $ OtherTCErr "Let currently unsupported"

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
    go (Pi dom cod) (x:xs) = do
        -- We don't actually care about the argument's type; just
        -- that it's compatible with the function's domain.
        _ <- check x dom
        
        let cod' = instantiate1 x cod
        go cod' xs

    -- No more arguments to check.
    go ty [] = pure ty
    
    -- Not a pi-type, but there are still some arguments left.
    go ty _ = throwTC $ 
        OtherTCErr $ "Expected pi-type, got " <> ppr hd <> " : " <> ppr ty
    
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
       
star, integer, double :: Type
star     = Star
integer  = Point $ Var 1 $ Just "integer"
double   = Point $ Var 2 $ Just "double"

integerKind, doubleKind :: Type
integerKind = star
doubleKind  = star

-- |
-- Matrix type constructor kind.
-- 
matrixTyConKind :: Type
matrixTyConKind
    = mkPi (varP 10) integer $ mkPi (varP 11) integer
    $ mkPi (varP 12) star $ star

-- |
-- Matrix type constructor.
--
matrixTyCon :: Type
matrixTyCon = Point $ Var 3 $ Just "matrix"
    
-- |
-- Matrix constructor type.
-- 
matrixTy :: Type
matrixTy
    = mkPi (varP 10) integer $ mkPi (varP 11) integer
    $ mkPi (varP 12) star
    $ App matrixTyCon [var 10, var 11, var 12]

-- |
-- Matrix constructor.
-- 
matrix :: Term
matrix = Point $ Var 4 $ Just "matrix"


-- |
-- Important constants:
--
--  (0) Type
--  (1) Int         : Type
--  (2) Double      : Type
--  (3) MatrixTyCon : Int -> Int -> Type -> Type
--  (4) Matrix      : (m n : Int) -> (t : Type) -> MatrixTyCon m n t
--  
-- We'll use these constants and their types for building and
-- typechecking everything else.
-- 
reallyPrimTys :: Map Name Term
reallyPrimTys = Map.fromList
    [ (1, integerKind       )
    , (2, doubleKind        )
    , (3, matrixTyConKind   )
    , (4, matrixTy          )
    ]
    
