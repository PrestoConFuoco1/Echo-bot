module Telegram.Exceptions where

import qualified Control.Monad.Catch as C

data TlException = TlException
    deriving (Show, Eq)

instance C.Exception TlException

