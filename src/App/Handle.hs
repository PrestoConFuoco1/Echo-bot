{-# LANGUAGE RankNTypes #-}
module App.Handle where

import Types
import qualified Data.Text as T (Text)
import Prelude hiding (log)
import qualified App.Logger as Logger
import qualified HTTPRequests as H
import BotClass.ClassTypes
import Data.Maybe (fromMaybe)

data Handle s m = Handle {
    log :: Logger.Handle m,

    commonEnv :: EnvironmentCommon,

    insertUser :: (BotClassTypes s) => User s -> Int -> m (),
    getUser :: (BotClassTypes s) => User s -> m (Maybe Int),

    getConstState :: (BotClassTypes s) => StateC s,

    getUpdates :: H.HTTPRequest -> m (Either String (UpdateResponse (RepSucc s) (RepErr s))),
    sendEcho :: H.HTTPRequest -> m (Either String (Rep s)),
    sendHelp :: H.HTTPRequest -> m (Either String (Rep s)),
    sendKeyboard :: H.HTTPRequest -> m (Either String (Rep s)),
    sendRepNumMessage :: H.HTTPRequest -> m (Either String (Rep s)),

    specH :: (BotClassTypes s) => Hndl s m
    }


findWithDefault :: (BotClassTypes s, Monad m) => Handle s m -> s -> Int -> Maybe (User s) -> m Int
findWithDefault h _ def mUser =
  case mUser of
    Nothing -> return def
    Just user -> do
        mRepNum <- getUser h user
        --return $ maybe def id mRepNum
        return $ fromMaybe def mRepNum

logDebug, logInfo, logWarning, logError, logFatal :: Handle s m -> T.Text -> m ()

logDebug h = Logger.logDebug (log h)
logInfo h = Logger.logInfo (log h)
logWarning h = Logger.logWarning (log h)
logError h = Logger.logError (log h)
logFatal h = Logger.logFatal (log h)

logEntry :: Handle s m -> Logger.LoggerEntry -> m ()
logEntry h (pri, msg) = Logger.log (log h) pri msg
