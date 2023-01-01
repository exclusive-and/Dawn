
-----------------------------------------------------------
-- |
-- Module       : Giskard.Pretty
-- Description  : Simple Prettyprinter Class
-----------------------------------------------------------

module Giskard.Pretty where

import              Data.Text (Text, pack)
import qualified    Data.Text as Text


class Ppr a where
    ppr :: a -> Text
