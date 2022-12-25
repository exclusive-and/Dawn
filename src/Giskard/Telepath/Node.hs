
module Giskard.Telepath.Node where

import Giskard.Names


data Node = Node
    { nodeId        :: Name
    , nodeKernel    :: [Value] -> Value
    , nodeInputs    :: [Name]
    }

data Value
