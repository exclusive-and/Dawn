
-----------------------------------------------------------
-- |
-- Module       : Giskard.Telepath.Ops
-- Description  : Operation API for Telepath
-----------------------------------------------------------
module Giskard.Telepath.Ops where

import              Giskard.Calculus.Term (Term' (..), mkPi)
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

-- |
-- Map of operation names to operation terms.
-- 
primOpTmMap :: Map OpName Term
primOpTmMap = Map.fromList
    [ ("Placeholder"    , Point $ Var 10 $ Just "Placeholder")
    , ("Ones"           , Point $ Var 11 $ Just "Ones"       )
    , ("Negate"         , Point $ Var 12 $ Just "Negate"     )
    , ("Add"            , Point $ Var 13 $ Just "Add"        )
    , ("Sub"            , Point $ Var 14 $ Just "Sub"        )
    , ("MatMul"         , Point $ Var 15 $ Just "MatMul"     )
    ]


-----------------------------------------------------------
-- Operation Frontend API
-----------------------------------------------------------

type NullaryOp = Telepath Node
type UnaryOp   = Node -> Telepath Node
type BinaryOp  = Node -> Node -> Telepath Node

placeholder :: Int -> Int -> Type -> NullaryOp
placeholder m n ty = newNode "Placeholder" [intLit m, intLit n, ty] []

ones :: Int -> Int -> Type -> NullaryOp
ones m n ty = newNode "Ones" [intLit m, intLit n, ty] []

neg :: UnaryOp
neg x = newNode "Negate" [] [x]

add :: BinaryOp
add x y = newNode "Add" [] [x, y]

sub :: BinaryOp
sub x y = newNode "Sub" [] [x, y]

matmul :: BinaryOp
matmul x y = newNode "MatMul" [] [x, y]

