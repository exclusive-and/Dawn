
module Test where

import Giskard.Calculus.Term
import Giskard.Calculus.Match
import Giskard.Calculus.Ppr
import Giskard.Names

import              Control.Monad.State.Lazy
import              Control.Monad.Trans.Except
import Data.IntSet
import qualified Data.IntSet as IntSet
import Data.Map
import qualified Data.Map as Map

main = do
    let tm1 :: Term
        tm1 = mkLam 3 (Point 0) $ mkLam 2 (Point 0) (App (Point 1) [Point 3])
        
        tm2 :: Term
        tm2 = mkLam 2 (Point 0) $ mkLam 3 (Point 0) (App (App (Point 1) [Point 2]) [Point 3])
    
    let m = runMatch $ matchTerms newMatchSub tm1 tm2
        s = runState (runExceptT m) $ MatchState Map.empty 4
    
    case fst s of
        Left err -> print err
        _ -> print $ show $ Map.map ppr $ matchSub $ snd s
