{-# LANGUAGE RankNTypes #-}

module App.Handle where

import qualified App.Logger as Logger
import BotClass.ClassTypes
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (Text)
import qualified HTTPRequests as H
import Prelude hiding (log)
import qualified Types as Y

data Handle s m =
   Handle
      { log :: Logger.Handle m
      , commonEnv :: Y.EnvironmentCommon
      , insertUser :: (BotClassTypes s) =>
                         User s -> Int -> m ()
      , getUser :: (BotClassTypes s) =>
                      User s -> m (Maybe Int)
      , getConstState :: (BotClassTypes s) =>
                            StateC s
      , getUpdates :: H.HTTPRequest -> m (Either String (Y.UpdateResponse (RepSucc s) (RepErr s)))
      , sendEcho :: H.HTTPRequest -> m (Either String (Rep s))
      , sendHelp :: H.HTTPRequest -> m (Either String (Rep s))
      , sendKeyboard :: H.HTTPRequest -> m (Either String (Rep s))
      , sendRepNumMessage :: H.HTTPRequest -> m (Either String (Rep s))
      , specH :: (BotClassTypes s) =>
                    Hndl s m
      }

findWithDefault ::
      (BotClassTypes s, Monad m)
   => Handle s m
   -> Int
   -> Maybe (User s)
   -> m Int
findWithDefault h def mUser =
   case mUser of
      Nothing -> pure def
      Just user -> fromMaybe def <$> getUser h user

logDebug, logInfo, logWarning, logError, logFatal ::
      Handle s m -> T.Text -> m ()
logDebug h = Logger.logDebug (log h)

logInfo h = Logger.logInfo (log h)

logWarning h = Logger.logWarning (log h)

logError h = Logger.logError (log h)

logFatal h = Logger.logFatal (log h)

logEntry :: Handle s m -> Logger.LoggerEntry -> m ()
logEntry h (pri, msg) = Logger.log (log h) pri msg
