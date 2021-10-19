{-# LANGUAGE
    FlexibleContexts
    #-}
module Test.Mock where


import Telegram
import Test.Hspec
import Types
import Data.IORef
import Handlers
import qualified HTTPRequests as H
import App.Handle as D
import Telegram.General
import BotClass.ClassTypes
import BotClass.ClassTypesTeleInstance
import BotClass.BotTeleInstance
import Execute
import Execute.Logic
import Data.Aeson
import Data.Text as T
import qualified Stuff as S



sendCounterCommon :: (BotClassTypes s) => s -> IORef Int -> Rep s -> IO (Either String (Rep s))
sendCounterCommon s ref rep = do
    modifyIORef ref (+1)
 --   return $ Right successReply
    return $ Right rep


mockInsertUserCommon :: (BotClassTypes s) => s -> IORef (Maybe (User s, Int)) -> User s -> Int -> IO ()
mockInsertUserCommon s ref user repnum =
    writeIORef ref $ Just (user, repnum)



mockGetUserCommon :: (BotClassTypes s, Eq (User s)) => s -> IORef (Maybe (User s, Int)) -> User s -> IO (Maybe Int)
mockGetUserCommon s ref user = do
    maybePair <- readIORef ref
    case maybePair of
        Nothing -> return Nothing
        Just (u, repnum) -> if u == user
                            then return $ Just repnum
                            else return Nothing


