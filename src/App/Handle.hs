-- {-# LANGUAGE DatatypeContexts #-}
module App.Handle where
import BotTypes

import qualified Data.Text as T
import Prelude hiding (log)
import qualified App.Logger as Logger
import qualified HTTPRequests as H
import qualified Data.ByteString.Lazy.Char8 as BSL (ByteString)
--import BotClass

data Handle s m = Handle {
    log :: Logger.Handle m,
    sendRequest :: Bool -> H.HTTPRequest -> m (Either String BSL.ByteString),

    commonEnv :: EnvironmentCommon,

    insertUser :: (BotClass s) => s -> User s -> m ()
    }



logDebug, logInfo, logWarning, logError :: Handle m -> T.Text -> m ()

logDebug h = Logger.logDebug (log h)
logInfo h = Logger.logInfo (log h)
logWarning h = Logger.logWarning (log h)
logError h = Logger.logError (log h)
logFatal h = Logger.logFatal (log h)

--data (Show a) => Some a = Some a


