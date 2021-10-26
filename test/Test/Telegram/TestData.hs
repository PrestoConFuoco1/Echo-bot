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
    userID = 0,
    userIsBot = False,
    userFirstName = "firstname",
    userLastName = Just "lastname",
    userUsername = Just "username"
    }


defaultChat = TlChat {
    chatID = 0
    }

defaultMessage = TlMessage {
    messageMessageID = defaultMessageID,
    messageFrom = Just defaultUser,
    messageDate = defaultDate,
    messageChat = defaultChat,
    messageText = defaultText,
    messageMediaGroupID = Nothing,
    messageAnimation = Nothing,
    messageAudio = Nothing,
    messageDocument = Nothing,
    messagePhoto = Nothing,
    messageSticker = Nothing,
    messageVideo = Nothing,
    messageVideoNote = Nothing,
    messageVoice = Nothing,
    messageCaption = Nothing,
    messageContact = Nothing,
    messageDice = Nothing,
    messageVenue = Nothing,
    messageLocation = Nothing,
    messagePoll = Nothing
    }

successReply :: TlReply
successReply = TlReply {
    replyOk = True
    , replyResult = Just $ String "success"
    , replyDescription = Nothing
    , replyErrorCode = Nothing
    }



sendHelpMessageMsg :: TlMessage
sendHelpMessageMsg = defaultMessage { messageText = Just $ helpCommand defStateGen }

buildUpdate :: TlEvent -> T.Text -> TlUpdate
buildUpdate event msg = TlUpdate {
    updateUpdateID = 0
    , updateEvent = event
    , updateValue = String msg
    }

sendHelpMessageUpd :: TlUpdate
sendHelpMessageUpd =
    let event = TEMsg $ sendHelpMessageMsg
    in  buildUpdate event "help request update"

sendKeyboardMessage :: TlMessage
sendKeyboardMessage = defaultMessage { messageText = Just $ setRepNumCommand defStateGen }

sendKeyboardMessageUpd = buildUpdate (TEMsg sendKeyboardMessage) "send repnum buttons request"


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
    callbackID = "callback_id"
    , callbackFrom = defaultUser
    , callbackData = Just $ "set " <> S.showT int
    , callbackMessage = Just sendKeyboardMessage
    }

setRepNumUpdate :: Int -> TlUpdate
setRepNumUpdate repnum = buildUpdate (TECallback $ setRepNumCallback repnum) "set repeat number update"

simpleMessage = defaultMessage { messageText = Just "hello, this is yoba" }

simpleMessageUpdate :: TlUpdate
simpleMessageUpdate = buildUpdate (TEMsg simpleMessage) "simple message to echo"



mediaGroupMessage = defaultMessage {
    messageMediaGroupID = Just "media group id"
    , messageAudio = Just TlAudio {
        audioFileID = "file identifier"
        }
    }

mediaGroupUpdate = buildUpdate (TEMsg mediaGroupMessage) "media group update"
