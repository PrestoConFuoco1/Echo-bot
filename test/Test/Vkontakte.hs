{-# LANGUAGE TypeApplications, DataKinds #-}
module Test.Vkontakte where

import Vkontakte
import Test.Hspec
import Types
import Data.IORef
import Handlers
import qualified HTTPRequests as H
import App.Handle as D
import BotClass.ClassTypes
import BotClass.ClassTypesVkInstance
import BotClass.BotVkInstance
import Execute
import Execute.Logic
import Data.Aeson
import Data.Text as T
import qualified Stuff as S
import Test.Vkontakte.TestData
import Test.Mock


testVkontakte :: Spec
testVkontakte = describe "vkontakte logic" $ do
    testHelpMessage
    testSetRepNumCommand
    testSetRepNum
    testSendEcho 7


withRandomIDVkHandle = defaultVkHandle {getRandomID = (return 0 :: IO Integer)}

testHelpMessage :: Spec
testHelpMessage = do
    describe "help message test" $ do
        it "should process help request" $ do
            testHelpMessage' `shouldReturn` 1
            
testHelpMessage' :: IO Int
testHelpMessage' = do
    ref <- newIORef 0
    let 
        helpSender = const $ sendCounterCommon @Vkontakte ref successReply
        handle = (defaultHandle @Vkontakte) {
                D.sendHelp = helpSender
                , D.specH = withRandomIDVkHandle
              }
    handleUpdate @Vkontakte handle sendHelpMessageUpd
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
    let keyboardSender = const $ sendCounterCommon @Vkontakte ref successReply
        handle = (defaultHandle @Vkontakte) {
            D.sendKeyboard = keyboardSender
            , D.specH = withRandomIDVkHandle
            }
    handleUpdate @Vkontakte handle sendKeyboardMessageUpd
    int <- readIORef ref
    return int

testSetRepNum :: Spec
testSetRepNum = do
    describe "set repeat number test" $ do
        it "should set repeat number and send info message" $ do
            testSetRepNum' 3 `shouldReturn` True

testSetRepNum' :: Int -> IO Bool
testSetRepNum' repnum = do
    ref <- newIORef 0
    refMap <- newIORef Nothing
    let infoMessageSender = const $ sendCounterCommon @Vkontakte ref successReply
        
        handle = (defaultHandle @Vkontakte) {
            D.sendRepNumMessage = infoMessageSender
            , D.insertUser = mockInsertUserCommon @Vkontakte refMap
            , D.specH = withRandomIDVkHandle
            }
    handleUpdate @Vkontakte handle $ setRepNumUpdate repnum
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
    let echoSender = const $ sendCounterCommon @Vkontakte ref successReply
        handle = (defaultHandle @Vkontakte) {
            D.sendEcho = echoSender
            , D.getUser = mockGetUserCommon @Vkontakte refMap
            , D.specH = withRandomIDVkHandle
            }
    handleUpdate @Vkontakte handle $ simpleMessageUpdate
    int <- readIORef ref
    return int
