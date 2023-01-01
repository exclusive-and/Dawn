
-----------------------------------------------------------
-- |
-- Module       : Giskard.Telepath.Ops
-- Description  : Operation API for Telepath
-----------------------------------------------------------
module Giskard.Telepath.Ops where

import              Giskard.Calculus.Term (Term' (..), mkPi, stripApps)
import              Giskard.Calculus.Ppr
import              Giskard.Names
import              Giskard.Telepath.Types
import              Giskard.Telepath.Graph

import              Control.Monad.Trans.Except
import              Data.Map (Map)
import qualified    Data.Map as Map
import              Data.Text (Text)

import              Numeric.LinearAlgebra (Matrix)
import qualified    Numeric.LinearAlgebra as M


-----------------------------------------------------------
-- Operation Types
-----------------------------------------------------------

-- |
-- (t : Type) -> t -> t
-- 
unaryOpTy :: Type
unaryOpTy = mkPi (varP 30) star $ mkPi (varP 31) (var 30) $ var 30

-- |
-- (t : Type) -> t -> t -> t
-- 
binaryOpTy :: Type
binaryOpTy
    = mkPi (varP 30) star
    $ mkPi (varP 31) (var 30) $ mkPi (varP 32) (var 30)
    $ var 30

-- |
-- (m k n : Int) -> (t : Type) -> Mat m k t -> Mat k n t -> Mat m n t
-- 
matmulTy :: Type
matmulTy
    = mkPi (varP 30) integer $ mkPi (varP 31) integer
    $ mkPi (varP 32) integer
    $ mkPi (varP 33) star
    $ mkPi (varP 34) (App matrixTyCon [var 30, var 31, var 33])
    $ mkPi (varP 35) (App matrixTyCon [var 31, var 32, var 33])
    $ App matrixTyCon [var 30, var 32, var 33]

-- |
-- Types of primitive operations.
-- 
primOpTys :: Map Name Type
primOpTys = Map.fromList
    [ (10, matrixTy     )
    , (11, matrixTy     )
    , (12, unaryOpTy    )
    , (13, binaryOpTy   )
    , (14, binaryOpTy   )
    , (15, matmulTy     )
    ]

placeholderTm, onesTm, negateTm, addTm, subTm, matmulTm :: Term

placeholderTm = Point $ Var 10 $ Just "Placeholder"
onesTm        = Point $ Var 11 $ Just "Ones"
negateTm      = Point $ Var 12 $ Just "Negate"
addTm         = Point $ Var 13 $ Just "Add"
subTm         = Point $ Var 14 $ Just "Sub"
matmulTm      = Point $ Var 15 $ Just "MatMul"



placeholderTmComp :: TermComputer
placeholderTmComp vals inputs = pure $ App placeholderTm vals

onesTmComp :: TermComputer
onesTmComp vals inputs = pure $ App onesTm vals

negateTmComp :: TermComputer
negateTmComp vals inputs = do
    ty <- case inputs of
        [x] -> infer x
        _   -> throwTC $ OtherTCErr "Negate expects one argument"
    pure $ App negateTm $ [ty] ++ inputs
    
addTmComp :: TermComputer
addTmComp vals inputs = do
    ty <- case inputs of
        [x, y] -> do
            ty <- infer x
            check y ty
        _ -> throwTC $ OtherTCErr "Add expects two arguments"
    pure $ App addTm $ [ty] ++ inputs

subTmComp :: TermComputer
subTmComp vals inputs = do
    ty <- case inputs of
        [x, y] -> do
            ty <- infer x
            check y ty
        _ -> throwTC $ OtherTCErr "Sub expects two arguments"
    pure $ App subTm $ [ty] ++ inputs

matmulTmComp :: TermComputer
matmulTmComp vals inputs = do
    (m, k, n, ty) <- case inputs of
        [x, y] -> do
            xty <- infer x
            (m, k, ty) <- case stripApps xty of
                (Point (Var 3 _), args)
                    | [m, k, ty] <- args -> pure (m, k, ty)
                _ -> throwTC $ OtherTCErr
                             $ "MatMul expects matrix, got " <> ppr xty
            
            yty <- infer y
            n <- case stripApps yty of
                (Point (Var 3 _), args)
                    | [_, n, _] <- args -> pure n
                _ -> throwTC $ OtherTCErr 
                             $ "MatMul expects matrix, got " <> ppr yty
            
            pure (m, k, n, ty)
            
        _ -> throwTC $ OtherTCErr "MatMul expects two arguments"
    
    pure $ App matmulTm $ [m, k, n, ty] ++ inputs

-- |
-- Map of operation names to term computers.
-- 
primOpTmMap :: Map OpName TermComputer
primOpTmMap = Map.fromList
    [ ("Placeholder"    , placeholderTmComp)
    , ("Ones"           , onesTmComp       )
    , ("Negate"         , negateTmComp     )
    , ("Add"            , addTmComp        )
    , ("Sub"            , subTmComp        )
    , ("MatMul"         , matmulTmComp     )
    ]


-----------------------------------------------------------
-- Operation Frontend API
-----------------------------------------------------------

type NullaryOp m = TelepathT m Node
type UnaryOp   m = Node -> TelepathT m Node
type BinaryOp  m = Node -> Node -> TelepathT m Node

placeholder :: Monad m => Int -> Int -> Type -> NullaryOp m
placeholder m n ty = newNode "Placeholder" [intLit m, intLit n, ty] []

ones :: Monad m => Int -> Int -> Type -> NullaryOp m
ones m n ty = newNode "Ones" [intLit m, intLit n, ty] []

neg :: Monad m => UnaryOp m
neg x = newNode "Negate" [] [x]

add :: Monad m => BinaryOp m
add x y = newNode "Add" [] [x, y]

sub :: Monad m => BinaryOp m
sub x y = newNode "Sub" [] [x, y]

matmul :: Monad m => BinaryOp m
matmul x y = newNode "MatMul" [] [x, y]

