module Exceptions where

import Control.Monad.Catch as CMC



data BotException =
    ParsedNoUpdates
    | FailedToParseUpdatesListFromResult
--    | FailedToParseReply
    | UnableToHandleError
    deriving (Show)

instance Exception BotException


data VkException =
    FASd
    deriving (Show)

instance Exception VkException
