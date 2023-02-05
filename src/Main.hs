
module Main where

import Asimov.Calculus
import Asimov.Pretty
import Asimov.Typechecking.Tc
import Asimov.Typechecking.Common
-- import Giskard.Typechecking.STLC
import Asimov.Typechecking.SystemF

import Control.Lens
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text

main :: IO ()
main = pure ()

test :: TcMonad SystemF [MetaVarRef SystemF]
test = do
    let x0 = Var 0 Nothing
        x1 = Var 1 Nothing
        x2 = Var 2 Nothing
        x3 = Var 3 Nothing
    
    mv1 <- newFlexiMetaVar
    mv2 <- newFlexiMetaVar
    let x4 = Point $ mkMetaVarPoint mv1
        x5 = Point $ mkMetaVarPoint mv2
        
        tm1 = mkLam x1 Star $ App x4 [Point x1, Point x2]
        tm2 = mkLam x3 Star $ App (App (Point x0) [Star]) [Point x3, x5]
    unifyMetaVars tm1 tm2
    
    mv1x <- readMetaVar mv1
    mv2x <- readMetaVar mv2
    
    pure [mv1x, mv2x]


{-

import Giskard.Calculus.Term
import Giskard.Calculus.Pretty.Term

import Giskard.Telepath.Types
import Giskard.Telepath.Graph
import Giskard.Telepath.Ops

import Giskard.Pretty

import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Data.Functor.Identity
import qualified Data.Map as Map

import Numeric.LinearAlgebra ((><))

import Debug.Trace


tcmTest :: TCM Type
tcmTest = do
    let m1 = Point $ Lit $ MatDoubleLit 7 4 $ (7 >< 4) [1..]
        m2 = Point $ Lit $ MatDoubleLit 4 9 $ (4 >< 9) [1..]
        a1 = App (var 15) [intLit 7, intLit 4, intLit 9, double, m1, m2]
        m3 = Point $ Lit $ MatDoubleLit 9 7 $ (9 >< 7) [1..]
        a2 = App (var 15) [intLit 7, intLit 9, intLit 7, double, a1, m3]
    infer (App a2 [])

runTcmTest :: IO ()
runTcmTest = do
    let (r, _) = runTC tcmTest (TCState (reallyPrimTys `Map.union` primOpTys) 30)
    case r of
        Left err -> print $ pretty err
        Right ty -> print $ pretty ty

telepathTest :: Monad m => TelepathT m Node
telepathTest = do
    x <- placeholder 7 4 double
    y <- placeholder 4 9 double
    matmul x y

runTelepathTest :: IO ()
runTelepathTest = do
    let s = TelepathState Map.empty primOpTmMap 0
        (r, _) = runIdentity $ runTelepathT telepathTest s
    case r of
        Left err   -> print err
        Right node -> print node
    
telepathTCMTest :: TelepathT TCM Type
telepathTCMTest = inferNode =<< telepathTest

runTelepathTCMTest :: IO ()
runTelepathTCMTest = do
    let tcs = TCState (reallyPrimTys `Map.union` primOpTys) 30
        tls = TelepathState Map.empty primOpTmMap 0
        (r1, _) = runTC (runTelepathT telepathTCMTest tls) tcs
    case r1 of
        Left err -> print $ pretty err
        Right (r2, _) ->
            case r2 of
                Left err -> print err
                Right ty -> print $ pretty ty

main :: IO ()
main = do
    runTcmTest

        -}
