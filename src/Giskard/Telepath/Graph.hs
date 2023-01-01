
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------
-- |
-- Module       : Giskard.Telepath.Graph
-- Description  : Telepath's Graph Building Monad
-----------------------------------------------------------
module Giskard.Telepath.Graph where

import              Giskard.Calculus.Term (Term' (..))
import              Giskard.Calculus.Pretty.Term
import              Giskard.Names
import              Giskard.Pretty
import              Giskard.Telepath.Types (Term, Type, P (..), Literal, TCMT (..), TCM, infer)

import              Control.Monad
import              Control.Monad.State.Lazy
import              Control.Monad.Trans.Class
import              Control.Monad.Trans.Except
import              Data.Functor.Identity
import              Data.Map (Map)
import qualified    Data.Map as Map
import              Data.Text (Text, pack, unpack, intercalate)


-----------------------------------------------------------
-- High-level Graph Nodes
-----------------------------------------------------------

-- |
-- A node symbolically represents an actual operation we want to execute
-- in a neural net. The symbolic graph representation allows the engine
-- to do abstract reasoning about our layouts, enabling autodifferentiation
-- and layout optimization.
-- 
data Node = Node
    { nodeId        :: Name      -- ^ This node's ID in the graph.
    , nodeOp        :: OpName    -- ^ The operation this node represents.
    , nodeInputs    :: [Name]    -- ^ List of node's input connections.
    , nodeValues    :: [Term]    -- ^ Some data the node stores.
    }
    
instance Show Node where
    show (Node nm op inputs vals) =
        unpack $ intercalate " "
            [ "Node"
            , pack (show nm)
            , op
            , pack (show inputs)
            , "[" <> intercalate ", " (map ppr vals) <> "]"
            ]
    
type OpName = Text

type TermComputer = [Term] -> [Term] -> TCM Term

type GradientT m = [Node] -> Node -> TelepathT m [Node]

type Gradient = GradientT Identity


-----------------------------------------------------------
-- Graph Building Monad
-----------------------------------------------------------

data TelepathState = TelepathState
    { nodeMap       :: Map Name Node
    , opTmMap       :: Map OpName TermComputer
    , nameSupply    :: Name
    }
    
-- |
-- Telepath session monad transformer.
-- 
newtype TelepathT m a = Telepath
    { unTelepath :: ExceptT Text (StateT TelepathState m) a }
    deriving ( Functor, Applicative, Monad
             , MonadState TelepathState
             )

instance MonadTrans TelepathT where
    lift = Telepath . lift . lift
             
-- |
-- Telepath session with no underlying monad.
-- 
type Telepath = TelepathT Identity

-- |
-- Throw an exception in a Telepath monad.
--
throwTelepath :: Monad m => Text -> TelepathT m a
throwTelepath = Telepath . throwE

-- |
-- 
--
runTelepathT
    :: Monad m
    => TelepathT m a
    -> TelepathState
    -> m (Either Text a, TelepathState)
runTelepathT tp = runStateT (runExceptT $ unTelepath tp)

-- |
-- Run a Telepath session without an underlying monad.
-- 
runTelepath
    :: Telepath a
    -> TelepathState
    -> (Either Text a, TelepathState)
runTelepath tp = runState (runExceptT $ unTelepath tp)

-- |
-- Telepath session with support for IO actions.
-- 
type TelepathIO = TelepathT IO

-- |
-- Run a Telepath session with IO actions.
-- 
runTelepathIO
    :: TelepathIO a
    -> TelepathState
    -> IO (Either Text a, TelepathState)
runTelepathIO tp = runStateT (runExceptT $ unTelepath tp)

instance Monad m => NameMonad (TelepathT m) where
    newName = do
        i <- nameSupply <$> get
        modify $ \ s -> s { nameSupply = i + 1 }
        return i


-- |
-- Create a node inside a Telepath monad.
--
newNode :: Monad m => OpName -> [Term] -> [Node] -> TelepathT m Node
newNode op internalData inputs = do
    newId <- newName
    let inputIds = map nodeId inputs
        newNode  = Node newId op inputIds internalData
    
    -- Add this node to the graph's internal map.
    oldMap <- nodeMap <$> get
    let newMap = Map.insert newId newNode oldMap
    modify $ \ s -> s { nodeMap = newMap }

    pure newNode


-- |
-- Lookup a name in a Telepath graph.
--
getNode :: Monad m => Name -> TelepathT m Node
getNode nm = do
    nodes <- nodeMap <$> get
    case Map.lookup nm nodes of
        Just node -> pure node
        Nothing   -> throwTelepath $ "Couldn't find node with ID " <> pack (show nm)
    

-----------------------------------------------------------
-- Graph Typechecking
-----------------------------------------------------------

-- |
-- Get the term corresponding to an Op in a Telepath context.
-- 
getOpTm :: OpName -> TelepathT TCM TermComputer
getOpTm opName = do
    opTmComps <- opTmMap <$> get
    case Map.lookup opName opTmComps of
        Just tmComp -> pure tmComp
        Nothing     -> throwTelepath $ "Couldn't find term for " <> opName

-- |
-- Compute the term corresponding to a node in a Telepath graph.
--
computeNodeTm :: Node -> TelepathT TCM Term
computeNodeTm node = do
    let inputs = nodeInputs node
    inputNodes <- mapM getNode inputs
    
    -- Compute the terms of each input
    let valueTms = nodeValues node
    inputTms <- mapM computeNodeTm inputNodes
    
    -- Lookup the term corresponding to this node's Op.
    opTmComp <- getOpTm $ nodeOp node
    lift $ opTmComp valueTms inputTms

-- |
-- Infer the type of a node in a Telepath graph.
-- 
inferNode :: Node -> TelepathT TCM Type
inferNode node = do
    nodeTm <- computeNodeTm node
    lift $ infer nodeTm
    
