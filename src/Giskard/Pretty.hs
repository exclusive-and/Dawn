
-----------------------------------------------------------
-- |
-- Module       : Giskard.Pretty
-- Description  : Simple Prettyprinter Class
-----------------------------------------------------------

module Giskard.Pretty
    ( Pretty (..)
    ) where

import Data.Text (Text, pack)


class Pretty a where
    pretty :: a -> Text


instance Pretty () where
    pretty () = "()"
    
instance Pretty Int where
    pretty = pack . show

instance Pretty Text where
    pretty = id
