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
    testMediaGroup

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

testMediaGroup :: Spec
testMediaGroup =
    describe "media group test" $ do
        it "should save media group unit and send no requests immediately" $
            testMediaGroup' `shouldReturn` 1

testMediaGroup' :: IO Int
testMediaGroup' = do
    --ref <- newIORef 0
    refMap <- newIORef []
    let handle = (defaultHandle Tele) {
            D.specH = defaultTelegramHandle {
                insertMediaGroupUnit = mockInsertMediaGroupUnit refMap
                }
            }
    handleUpdate handle Tele $ mediaGroupUpdate
    int <- fmap Prelude.length $ readIORef refMap
    return int
    
{-
    result: 
        [{"update_id":332501497,"message":{"from":{"first_name":"Yuri","username":"r
        ozovyi_avtobyc","is_bot":false,"last_name":"Romanowski","id":380847769,"lang
        uage_code":"ru"},"chat":{"first_name":"Yuri","username":"rozovyi_avtobyc","l
        ast_name":"Romanowski","id":380847769,"type":"private"},"media_group_id":"13
        078204525058474","message_id":2435,"date":1634775565,"audio":{"file_id":"CQA
        CAgIAAxkBAAIJg2Fwsg1NuihCkyJzeMNq-KKNka-1AAKnEgAC9NqJS7fQ3SnOgcvvIQQ","mime_
        type":"audio/mpeg","file_size":9095212,"file_unique_id":"AgADpxIAAvTaiUs","t
        itle":"Autumn Leaves (Les Feuilles Mortes) (Sefon.Pro)","duration":226,"perf
        ormer":"Paul Mauriat","file_name":"0 Paul Mauriat - Autumn Leaves (Les Feuil
        les Mortes).mp3"}}},{"update_id":332501498,"message":{"from":{"first_name":"
        Yuri","username":"rozovyi_avtobyc","is_bot":false,"last_name":"Romanowski","
        id":380847769,"language_code":"ru"},"chat":{"first_name":"Yuri","username":"
        rozovyi_avtobyc","last_name":"Romanowski","id":380847769,"type":"private"},"
        media_group_id":"13078204525058474","message_id":2436,"date":1634775565,"aud
        io":{"file_id":"CQACAgIAAxkBAAIJhGFwsg107_Vpom7GjBbA8MeZlMGhAAKoEgAC9NqJS31D
        aidnEryJIQQ","mime_type":"audio/mpeg","file_size":8332744,"file_unique_id":"
        AgADqBIAAvTaiUs","title":"Ci Sarа (Sefon.me)","duration":206,"performer":"Al
         Bano & Romina Power","file_name":"1 Al Bano & Romina Power - Ci Sarа.mp3"}}
        }]
-}

{-
[Debug]: {Array}
    [0]: {TlMediaGroupPair}
        identifier: {TlMediaGroupIdentifier}
            chat: {TlChat}
                id: 380847769
            user: {TlUser}
                id: 380847769
                is_bot: False
                first_name: "Yuri"
                last_name: "Romanowski"
                username: "rozovyi_avtobyc"
            mediaGroupID: "13078204525058474"
        items: {Array}
            [0]: {TlInputMediaAudio}
                media: "CQACAgIAAxkBAAIJhGFwsg107_Vpom7GjBbA8MeZlMGhAAKoEgAC9NqJS31DaidnEryJIQQ"
            [1]: {TlInputMediaAudio}
                media: "CQACAgIAAxkBAAIJg2Fwsg1NuihCkyJzeMNq-KKNka-1AAKnEgAC9NqJS7fQ3SnOgcvvIQQ"
-}
