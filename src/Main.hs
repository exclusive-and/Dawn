
module Main where

import Giskard.Calculus.Term hiding (Term, Type)
import Giskard.Calculus.Match
import Giskard.Calculus.Ppr

import Giskard.Telepath.Types

import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Data.Map (Map)
import qualified Data.Map as Map

import Numeric.LinearAlgebra hiding (double, (<>))
import qualified Numeric.LinearAlgebra as M


tcmTest :: TCM Type
tcmTest = do
    let m1 = Point $ Lit $ MatDoubleLit 7 4 $ (7 >< 4) [1..]
        m2 = Point $ Lit $ MatDoubleLit 4 9 $ (4 >< 9) [1..]
        a1 = App (var 5) [intLit 7, intLit 4, intLit 9, double, m1, m2]
        m3 = Point $ Lit $ MatDoubleLit 9 7 $ (9 >< 7) [1..]
        a2 = App (var 5) [intLit 7, intLit 9, intLit 7, double, a1, m3]
    infer a2

main :: IO ()
main = do
    let (r, s) = runState (runExceptT $ runTC tcmTest) (TCState constants 10)
        
    case r of
        Left err  ->
            case err of
                Mismatch ty1 ty2 -> print $ "Mismatch between " <> ppr ty1 <> " and " <> ppr ty2
                NotInScope x     -> print $ "Variable " <> show x <> " not in scope"
                OtherTCErr err   -> print err
        Right ty  -> print $ ppr ty
