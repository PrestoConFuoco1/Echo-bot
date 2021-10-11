module Exceptions where

import Control.Monad.Catch as CMC



data BotException =
    ParsedNoUpdates
    | FailedToParseUpdatesListFromResult
    | FailedToParseReply
    deriving (Show)

instance Exception BotException
