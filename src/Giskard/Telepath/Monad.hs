
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Giskard.Telepath.Monad where

import Giskard.Names
import Giskard.Telepath.Node

import Control.Monad
import Control.Monad.State.Lazy
import Data.Functor.Identity
import Data.Map (Map)


data TelepathState = TelepathState
    { nodeMap       :: Map Name Node
    , nameSupply    :: Name
    }
    
-- |
-- Telepath session monad transformer.
-- 
newtype TelepathT m a = Telepath { runTelepathT :: StateT TelepathState m a }
    deriving (Functor, Applicative, Monad)

-- |
-- Telepath session with no underlying monad.
-- 
type Telepath = TelepathT Identity

-- |
-- Run a Telepath session without an underlying monad.
-- 
runTelepath :: Telepath a -> TelepathState -> (a, TelepathState)
runTelepath tp = runState (runTelepathT tp)

-- |
-- Telepath session with support for IO actions.
-- 
type TelepathIO = TelepathT IO

-- |
-- Run a Telepath session with IO actions.
-- 
runTelepathIO :: TelepathIO a -> TelepathState -> IO (a, TelepathState)
runTelepathIO tp = runStateT (runTelepathT tp)

instance Monad m => NameMonad (TelepathT m) where
    newName = TelepathT $ do
        i <- nameSupply <$> get
        modify $ \ s -> s { nameSupply = i + 1 }
        return i
