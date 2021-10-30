module Exceptions where

import Control.Monad.Catch as CMC

data BotException
    = ParsedNoUpdates
    | FailedToParseUpdatesListFromResult
    | UnableToHandleError
  deriving (Show)

instance Exception BotException
