
module Asimov.Names where

type Name = Int

class Monad m => NameMonad m where
    newName :: m Name
