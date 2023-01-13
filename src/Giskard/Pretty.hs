
-----------------------------------------------------------
-- |
-- Module       : Giskard.Pretty
-- Description  : Simple Prettyprinter Class
-----------------------------------------------------------

module Giskard.Pretty where

import Data.Text (Text, pack)


class Ppr a where
    ppr :: a -> Text


instance Ppr () where
    ppr () = "()"
    
instance Ppr Int where
    ppr = pack . show

instance Ppr Text where
    ppr = id
