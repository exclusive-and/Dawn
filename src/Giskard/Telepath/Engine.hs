
-----------------------------------------------------------
-- |
-- Module       : Giskard.Telepath.Engine
-- Description  : Telepath Graph Execution Engine
-----------------------------------------------------------
module Giskard.Telepath.Engine where

import Giskard.Names
import Giskard.Telepath.Monad
import Giskard.Telepath.Node

import Control.Monad
import Control.Monad.State.Lazy
import GHC.Arr


data ExGraph = ExGraph
    { exNodes   :: ExNodeArray
    , exPending :: ExPendingArray
    }

data ExNode = ExNode
    { exNodeId      :: Name
    , exNodeKernel  :: [Value] -> Value
    , exNodeOutputs :: [Name]
    }
    
type ExNodeArray = Array Int ExNode

type ExPendingArray = Array Int Int



