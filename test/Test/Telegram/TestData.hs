module Test.Telegram.TestData where

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



sendHelpMessageMsg :: TlMessage
sendHelpMessageMsg = defaultMessage { _TM_text = Just $ helpCommand defStateGen }

buildUpdate :: TlEvent -> T.Text -> TlUpdate
buildUpdate event msg = TlUpdate {
    _TU_update_id = 0
    , _TU_event = event
    , _TU_value = String msg
    }

sendHelpMessageUpd :: TlUpdate
sendHelpMessageUpd =
    let event = TEMsg $ sendHelpMessageMsg
    in  buildUpdate event "help request update"
{-
sendHelpMessageUpd :: TlUpdate
sendHelpMessageUpd =
    let event = TEMsg $ sendHelpMessageMsg
    in  TlUpdate {
        _TU_update_id = 0
        , _TU_event = event
        , _TU_value = String "help request update"
        }
-}


sendKeyboardMessage :: TlMessage
sendKeyboardMessage = defaultMessage { _TM_text = Just $ setRepNumCommand defStateGen }

sendKeyboardMessageUpd = buildUpdate (TEMsg sendKeyboardMessage) "set repnum buttons request"


{-
{"text":"Now every your message will be repeated 2 times.","from":{"first_na
        me":"HaskellTerminator666","username":"haskell289_bot","is_bot":true,"id":11
        01304154},"chat":{"first_name":"Yuri","username":"rozovyi_avtobyc","last_nam
        e":"Romanowski","id":380847769,"type":"private"},"message_id":1896,"date":16
        34657428}
-}
{-
[{"callback_query":{"data":"set 1","from":{"first_name":"Yuri","username":"r
        ozovyi_avtobyc","is_bot":false,"last_name":"Romanowski","id":380847769,"lang
        uage_code":"ru"},"id":"1635728713267040307","chat_instance":"138084231665789
        4230","message":{"text":"How many times would you like to repeat every reply
        ?","from":{"first_name":"HaskellTerminator666","username":"haskell289_bot","
        is_bot":true,"id":1101304154},"chat":{"first_name":"Yuri","username":"rozovy
        i_avtobyc","last_name":"Romanowski","id":380847769,"type":"private"},"messag
        e_id":1892,"date":1634591433,"reply_markup":{"inline_keyboard":[[{"text":"1"
        ,"callback_data":"set 1"},{"text":"2","callback_data":"set 2"},{"text":"3","
        callback_data":"set 3"},{"text":"4","callback_data":"set 4"},{"text":"5","ca
        llback_data":"set 5"}]]}}},"update_id":332501240}]
-}


setRepNumCallback :: Int -> TlCallback
setRepNumCallback int = TlCallback {
    _TCB_id = "callback_id"
    , _TCB_from = defaultUser
    , _TCB_data = Just $ "set " <> S.showT int
    , _TCB_message = Just sendKeyboardMessage
    }

setRepNumUpdate :: Int -> TlUpdate
setRepNumUpdate repnum = buildUpdate (TECallback $ setRepNumCallback repnum) "set repeat number update"

simpleMessage = defaultMessage { _TM_text = Just "hello, this is yoba" }

simpleMessageUpdate :: TlUpdate
simpleMessageUpdate = buildUpdate (TEMsg simpleMessage) "simple message to echo"


