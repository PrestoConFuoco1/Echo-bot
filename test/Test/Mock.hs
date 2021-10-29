{-# LANGUAGE
    FlexibleContexts
    , AllowAmbiguousTypes
    #-}
module Test.Mock where


import Telegram
import Test.Hspec
import Data.IORef
import Handlers
import qualified HTTP.Types as H
import App.Handle as D
import Telegram
import BotTypesClass.ClassTypes
import BotTypesClass.TelegramInstance
import Execute.Telegram
import Execute
import Execute.Logic
import Data.Aeson
import Data.Text as T
import qualified Stuff as S



sendCounterCommon :: (BotClassTypes s) => IORef Int -> Rep s -> IO (Either String (Rep s))
sendCounterCommon ref rep = do
    modifyIORef ref (+1)
 --   return $ Right successReply
    return $ Right rep


mockInsertUserCommon :: (BotClassTypes s) => IORef (Maybe (User s, Int)) -> User s -> Int -> IO ()
mockInsertUserCommon ref user repnum =
    writeIORef ref $ Just (user, repnum)



mockGetUserCommon :: (BotClassTypes s, Eq (User s)) => IORef (Maybe (User s, Int)) -> User s -> IO (Maybe Int)
mockGetUserCommon ref user = do
    maybePair <- readIORef ref
    case maybePair of
        Nothing -> return Nothing
        Just (u, repnum) -> if u == user
                            then return $ Just repnum
                            else return Nothing


mockInsertMediaGroupUnit ::
    IORef [(TlMediaGroupIdentifier, TlMediaGroupUnit)]
    -> TlMediaGroupIdentifier
    -> TlMediaGroupUnit -> IO ()
mockInsertMediaGroupUnit ref ident unit = do
    modifyIORef ref ((ident, unit) :)

