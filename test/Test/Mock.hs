{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}

module Test.Mock where

import App.BotHandler as D
import BotTypesClass.ClassTypes
import BotTypesClass.TelegramInstance
import Data.Aeson
import Data.IORef
import Data.Text as T
import Execute
import Execute.Logic
import Execute.Telegram
import qualified HTTP.Types as H
import Handlers
import qualified Stuff as S
import Telegram
import Test.Hspec

sendCounterCommon ::
     (BotClassTypes s) => IORef Int -> Rep s -> IO (Either String (Rep s))
sendCounterCommon ref rep = do
  modifyIORef ref (+ 1)
 --   return $ Right successReply
  return $ Right rep

mockInsertUserCommon ::
     (BotClassTypes s) => IORef (Maybe (User s, Int)) -> User s -> Int -> IO ()
mockInsertUserCommon ref user repnum = writeIORef ref $ Just (user, repnum)

mockGetUserCommon ::
     (BotClassTypes s, Eq (User s))
  => IORef (Maybe (User s, Int))
  -> User s
  -> IO (Maybe Int)
mockGetUserCommon ref user = do
  maybePair <- readIORef ref
  case maybePair of
    Nothing -> return Nothing
    Just (u, repnum) ->
      if u == user
        then return $ Just repnum
        else return Nothing

mockInsertMediaGroupUnit ::
     IORef [(TlMediaGroupIdentifier, TlMediaGroupUnit)]
  -> TlMediaGroupIdentifier
  -> TlMediaGroupUnit
  -> IO ()
mockInsertMediaGroupUnit ref ident unit = do
  modifyIORef ref ((ident, unit) :)
