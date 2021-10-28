{-# LANGUAGE RankNTypes #-}

module App.Handle where

import qualified App.Logger as L
import BotTypesClass.ClassTypes (BotClassTypes (..))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (Text)
import qualified HTTPTypes as H
import Prelude hiding (log)
import qualified Environment as Env

data BotHandler s m = BotHandler
  { log :: L.LoggerHandler m,
    commonEnv :: Env.EnvironmentCommon,
    insertUser ::
      (BotClassTypes s) =>
      User s ->
      Int ->
      m (),
    getUser ::
      (BotClassTypes s) =>
      User s ->
      m (Maybe Int),
    getConstState ::
      (BotClassTypes s) =>
      StateC s,
    getUpdates :: H.HTTPRequest -> m (Either String (Either (RepErr s) (RepSucc s))),
    sendEcho :: H.HTTPRequest -> m (Either String (Rep s)),
    sendHelp :: H.HTTPRequest -> m (Either String (Rep s)),
    sendKeyboard :: H.HTTPRequest -> m (Either String (Rep s)),
    sendRepNumMessage :: H.HTTPRequest -> m (Either String (Rep s)),
    specH ::
      (BotClassTypes s) =>
      Hndl s m
  }

findWithDefault ::
  (BotClassTypes s, Monad m) =>
  BotHandler s m ->
  Int ->
  Maybe (User s) ->
  m Int
findWithDefault h def mUser =
  case mUser of
    Nothing -> pure def
    Just user -> fromMaybe def <$> getUser h user

logDebug,
  logInfo,
  logWarning,
  logError,
  logFatal ::
    BotHandler s m -> T.Text -> m ()
logDebug h = L.logDebug (log h)
logInfo h = L.logInfo (log h)
logWarning h = L.logWarning (log h)
logError h = L.logError (log h)
logFatal h = L.logFatal (log h)

logEntry :: BotHandler s m -> L.LoggerEntry -> m ()
logEntry h (pri, msg) = L.log (log h) pri msg
