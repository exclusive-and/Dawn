
module Main where

import Giskard.Calculus.Term hiding (Term, Type)
import Giskard.Calculus.Match
import Giskard.Calculus.Ppr

import Giskard.Telepath.Types

import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Data.Map (Map)
import qualified Data.Map as Map


tcmTest :: TCM Type
tcmTest = infer $ App matrix [Point $ Lit $ IntLit 7, Point $ Lit $ IntLit 7, double]

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
