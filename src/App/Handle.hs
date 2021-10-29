{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module App.Handle
  ( BotHandler (..),
    HasBotHandler (..),
    logDebug,
    logError,
    logFatal,
    logWarning,
    logInfo,
    logEntry,
    findWithDefault,
  )
where

import qualified App.Logger as L
import BotTypesClass.ClassTypes (BotClassTypes (..))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (Text)
import qualified Environment as Env
import qualified HTTP.Types as H
import qualified Messenger as M
import Prelude hiding (log)

class HasBotHandler (s :: M.Messenger) where
  type StateC s :: *
  type StateM s :: *
  type Hndl s :: (* -> *) -> *

data BotHandler s m = BotHandler
  { log :: L.LoggerHandler m,
    commonEnv :: Env.Environment,
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
      (HasBotHandler s) =>
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
