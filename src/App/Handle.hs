{-# LANGUAGE RankNTypes #-}
module App.Handle where

import Types

import qualified Data.Text as T
import Prelude hiding (log)
import qualified App.Logger as Logger
import qualified HTTPRequests as H
import qualified Data.ByteString.Lazy.Char8 as BSL (ByteString)
import BotClass.ClassTypes

data Handle s m = Handle {
    log :: Logger.Handle m,
    sendRequest :: Bool -> H.HTTPRequest -> m (Either String BSL.ByteString),

    commonEnv :: EnvironmentCommon,

    insertUser :: (BotClassTypes s) => User s -> Int -> m (),
    getUser :: (BotClassTypes s) => User s -> m (Maybe Int),

    getConstState :: (BotClassTypes s) => s -> StateC s,

--    getMutState :: (BotClassTypes s) => s -> m (StateM s),
--    putMutState :: (BotClassTypes s) => s -> StateM s -> m (),

    specH :: (BotClassTypes s) => Hndl s m
    }

    


findWithDefault :: (BotClassTypes s, Monad m) => Handle s m -> s -> Int -> Maybe (User s) -> m Int
findWithDefault h s def mUser =
  case mUser of
    Nothing -> return def
    Just user -> do
        mRepNum <- getUser h user
        return $ maybe def id mRepNum
{-
modifyMutState :: (BotClassTypes s, Monad m) => Handle s m -> s -> (StateM s -> StateM s) -> m ()
modifyMutState h s func = do
    oldState <- getMutState h s
    putMutState h s (func oldState)

withMutState :: (BotClassTypes s, Monad m) => Handle s m -> s -> (StateM s -> (a, StateM s)) -> m a
withMutState h s func = do
    oldState <- getMutState h s
    let (res, newState) = func oldState
    putMutState h s newState
    return res
-}

logDebug, logInfo, logWarning, logError :: Handle s m -> T.Text -> m ()

logDebug h = Logger.logDebug (log h)
logInfo h = Logger.logInfo (log h)
logWarning h = Logger.logWarning (log h)
logError h = Logger.logError (log h)
logFatal h = Logger.logFatal (log h)

--data (Show a) => Some a = Some a

logEntry :: Handle s m -> Logger.LoggerEntry -> m ()
logEntry h (pri, msg) = Logger.log (log h) pri msg
