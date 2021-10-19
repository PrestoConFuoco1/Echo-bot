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

defaultMessageID = 0
defaultDate = 0
defaultText = Nothing
defaultUser = TlUser {
    _TUs_id = 0,
    _TUs_is_bot = False,
    _TUs_first_name = "firstname",
    _TUs_last_name = Just "lastname",
    _TUs_username = Just "username"
    }


defaultChat = TlChat {
    _TC_id = 0
    }

defaultMessage = TlMessage {
    _TM_message_id = defaultMessageID,
    _TM_from = Just defaultUser,
    _TM_date = defaultDate,
    _TM_chat = defaultChat,
    _TM_text = defaultText,
    _TM_media_group_id = Nothing,
    _TM_animation = Nothing,
    _TM_audio = Nothing,
    _TM_document = Nothing,
    _TM_photo = Nothing,
    _TM_sticker = Nothing,
    _TM_video = Nothing,
    _TM_video_note = Nothing,
    _TM_voice = Nothing,
    _TM_caption = Nothing,
    _TM_contact = Nothing,
    _TM_dice = Nothing,
    _TM_venue = Nothing,
    _TM_location = Nothing
 
    }

successReply :: TlReply
successReply = TlReply {
    _TR_ok = True
    , _TR_result = Just $ String "success"
    , _TR_description = Nothing
    , _TR_error_code = Nothing
    }

sendCounter :: IORef Int -> H.HTTPRequest -> IO (Either String TlReply)
sendCounter ref _ = do
    modifyIORef ref (+1)
    return $ Right successReply

sendHelpMessageMsg :: TlMessage
sendHelpMessageMsg = defaultMessage { _TM_text = Just $ helpCommand defStateGen }

sendHelpMessageUpd :: TlUpdate
sendHelpMessageUpd =
    let event = TEMsg $ sendHelpMessageMsg
    in  TlUpdate {
        _TU_update_id = 0
        , _TU_event = event
        , _TU_value = String "help request update"
        }



testHelpMessage :: Spec
testHelpMessage = do
    describe "help message test" $ do
        it "should process help request" $ do
            testHelpMessage' `shouldReturn` 1
            
testHelpMessage' :: IO Int
testHelpMessage' = do
    ref <- newIORef 0
    let helpSender = sendCounter ref
        handle = (defaultHandle Tele) { D.sendHelp = helpSender }
    handleUpdate handle Tele sendHelpMessageUpd
    int <- readIORef ref
    return int

testTelegram :: Spec
testTelegram = do
    undefined
