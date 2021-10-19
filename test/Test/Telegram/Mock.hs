module Test.Telegram.Mock where


import Test.Telegram.TestData
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




sendCounter :: IORef Int -> IO (Either String TlReply)
sendCounter ref = do
    modifyIORef ref (+1)
    return $ Right successReply


mockInsertUser :: IORef (Maybe (TlUser, Int)) -> TlUser -> Int -> IO ()
mockInsertUser ref user repnum =
    writeIORef ref $ Just (user, repnum)


mockGetUser :: IORef (Maybe (TlUser, Int)) -> TlUser -> IO (Maybe Int)
mockGetUser ref user = do
    maybePair <- readIORef ref
    case maybePair of
        Nothing -> return Nothing
        Just (u, repnum) -> if u == user
                            then return $ Just repnum
                            else return Nothing


