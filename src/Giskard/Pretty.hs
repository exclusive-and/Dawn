
-----------------------------------------------------------
-- |
-- Module       : Giskard.Pretty
-- Description  : Simple Prettyprinter Class
-----------------------------------------------------------

module Giskard.Pretty where

import              Data.Text (Text)


class Ppr a where
    ppr :: a -> Text
