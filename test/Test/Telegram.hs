module Test.Telegram where

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
import Test.Telegram.TestData
import Test.Mock


testTelegram :: Spec
testTelegram = describe "telegram logic" $ do
    testHelpMessage
    testSetRepNumCommand
    testSetRepNum
    testSendEcho (-1)


testHelpMessage :: Spec
testHelpMessage = do
    describe "help message test" $ do
        it "should process help request" $ do
            testHelpMessage' `shouldReturn` 1
            
testHelpMessage' :: IO Int
testHelpMessage' = do
    ref <- newIORef 0
    let helpSender = const $ sendCounterCommon Tele ref successReply
        handle = (defaultHandle Tele) { D.sendHelp = helpSender }
    handleUpdate handle Tele sendHelpMessageUpd
    int <- readIORef ref
    return int


testSetRepNumCommand :: Spec
testSetRepNumCommand = do
    describe "send repnum keyboard message test" $ do
        it "should send the keyboard after rep num request" $ do
            testSetRepNumCommand' `shouldReturn` 1

testSetRepNumCommand' :: IO Int
testSetRepNumCommand' = do
    ref <- newIORef 0
    let keyboardSender = const $ sendCounterCommon Tele ref successReply
        handle = (defaultHandle Tele) { D.sendKeyboard = keyboardSender }
    handleUpdate handle Tele sendKeyboardMessageUpd
    int <- readIORef ref
    return int

testSetRepNum :: Spec
testSetRepNum = do
    describe "set repeat number test" $ do
        it "set repeat number and send info message" $ do
            testSetRepNum' 3 `shouldReturn` True

testSetRepNum' :: Int -> IO Bool
testSetRepNum' repnum = do
    ref <- newIORef 0
    refMap <- newIORef Nothing :: IO (IORef (Maybe (TlUser, Int)))
    let infoMessageSender = const $ sendCounterCommon Tele ref successReply
        
        handle = (defaultHandle Tele) {
            D.sendRepNumMessage = infoMessageSender
            , D.insertUser = mockInsertUserCommon Tele refMap
            }
    handleUpdate handle Tele $ setRepNumUpdate repnum
    int <- readIORef ref
    maybeUserRepnum <- readIORef refMap
    let bool = int == 1 && maybeUserRepnum == Just (defaultUser, repnum)
    return bool
-- if this test doesn't pass, use S.echo from Stuff.hs

testSendEcho :: Int -> Spec
testSendEcho int = do
    describe "message echo test" $ do
        it "should send echo message proper number of times" $ do
            testSendEcho' int `shouldReturn` validateRepNum int

testSendEcho' :: Int -> IO Int
testSendEcho' repnum = do
    ref <- newIORef 0
    refMap <- newIORef $ Just (defaultUser, repnum)
    let echoSender = const $ sendCounterCommon Tele ref successReply
        handle = (defaultHandle Tele) {
            D.sendEcho = echoSender
            , D.getUser = mockGetUserCommon Tele refMap
            }
    handleUpdate handle Tele $ simpleMessageUpdate
    int <- readIORef ref
    return int
