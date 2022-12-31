
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------
-- |
-- Module       : Giskard.Calculus.Match
-- Description  : Pattern Matching for Giskard Calculus
-----------------------------------------------------------
module Giskard.Calculus.Match where

import              Giskard.Calculus.Term
import              Giskard.Names

import              Control.Monad
import              Control.Monad.State.Lazy
import              Control.Monad.Trans.Except
import              Data.IntSet (IntSet)
import qualified    Data.IntSet as IntSet
import              Data.Map (Map)
import qualified    Data.Map as Map

-----------------------------------------------------------
-- Pattern Matching
-----------------------------------------------------------

data MatchState = MatchState
    { matchSub :: Subst Name
    , idSupply :: Int
    }

newtype MatchM r = MatchM { runMatch :: ExceptT String (State MatchState) r }
    deriving (Functor, Applicative, Monad)

instance NameMonad MatchM where
    newName = MatchM $ do
        i <- idSupply <$> get
        modify $ \ s -> s { idSupply = i + 1 }
        return i

-- |
-- Try to match a term as a pattern with another term.
-- 
matchTerms
    :: (a -> Term' c -> MatchM ())  -- ^ Extensible point matching function.
    -> Term' a                      -- ^ Pattern term.
    -> Term' c                      -- ^ Term to match with.
    -> MatchM ()

matchTerms matchPoint pat tm = case (pat, tm) of
    (Point p      , _         ) -> matchPoint p tm

    (Pi pdom pcod , Pi dom cod) -> do
        matchTerms matchPoint pdom dom
        matchAbs matchPoint pcod cod

    (Lam pdom pe  , Lam dom e ) -> do
        matchTerms matchPoint pdom dom
        matchAbs matchPoint pe e
        
    (App pf px    , App f x   ) -> do
        -- Argument matching must be done outside-in.
        let px' = reverse px
            x'  = reverse x
        matchFunApps matchPoint pf f px' x'
    
    (_            , _         ) ->
        MatchM $ throwE "Match terms failed!"

-- |
-- Try to match two abstractions. Matches bound points correctly using
-- syntactic equality.
-- 
matchAbs
    :: SynEq b
    => (a -> Term' c -> MatchM ())  -- ^ Extensible point matching function.
    -> Abs b Term' a                -- ^ Pattern abstraction.
    -> Abs b Term' c                -- ^ Abstraction to match with.
    -> MatchM ()

matchAbs f (Abs pm) (Abs m) = matchTerms f' pm m where
    -- A bound point should only match with another syntactically equal
    -- bound point.
    f' (Bound pb) tm
        | (Point (Bound b)) <- tm, synEq pb b = return ()
        | otherwise = MatchM $ throwE "Expected BV, got FV"
    
    -- If a point is a subterm, then the subterm contains no variables
    -- bound by this abstraction. Therefore, the subterm we're matching
    -- with should also contain no bound variables.
    f' (Subterm psub) tm = do
        sub <- checkReallyFree tm
        matchTerms f psub sub

    checkReallyFree :: Term' (Point b (Term' c)) -> MatchM (Term' c)
    checkReallyFree = fmap join . traverse goReallyFree where
        goReallyFree (Bound   b) = MatchM $ throwE "Expected FV, got BV"
        goReallyFree (Subterm a) = return a

-- |
-- Try to match two application stacks. Succeeds if all terms in the
-- first stack match against a term in the second stack.
-- 
matchFunApps
    :: (a -> Term' c -> MatchM ())
    -> Term' a
    -> Term' c
    -> [Term' a]
    -> [Term' c]
    -> MatchM ()
    
matchFunApps matchPoint pf f (px:pxs) (x:xs) = do
    matchTerms matchPoint px x
    matchFunApps matchPoint pf f pxs xs
    
matchFunApps matchPoint pf f [] xs =
    matchTerms matchPoint pf (App f xs)

matchFunApps matchPoint pf f _ [] =
    MatchM $ throwE "Couldn't match application stacks"
        
newMatchSub :: Name -> Term -> MatchM ()
newMatchSub name tm = MatchM $ do
    oldMap <- matchSub <$> get
    let old   = Map.lookup name oldMap
        addTm = modify $ \ s -> s { matchSub = Map.insert name tm oldMap }
    case old of
        Nothing -> addTm
        Just oldTm
            | synEq tm oldTm -> addTm
            | otherwise      -> throwE "Match failed!"
