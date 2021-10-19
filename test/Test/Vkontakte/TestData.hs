module Test.Vkontakte.TestData where

import Test.Hspec
import Types
import Data.IORef
import Handlers
import qualified HTTPRequests as H
import App.Handle as D
import Vkontakte
import BotClass.ClassTypes
import BotClass.ClassTypesVkInstance
import BotClass.BotVkInstance
import Execute
import Execute.Logic
import Data.Aeson
import Data.Text as T
import qualified Stuff as S


{-
data VkMessage = VkMessage {
    _VM_id :: Integer,
    _VM_from_id :: VkUser,
    _VM_text :: Maybe T.Text,
    _VM_attachments :: [VkAttachment]
    --_VM_payload :: Maybe VkMyCallback
    } deriving (Eq, Show, Generic)


-}

defaultUser = VkUser { _VU_id = 0 }

defaultMessage = VkMessage {
    _VM_id = 0
    , _VM_from_id = defaultUser
    , _VM_text = Nothing
    , _VM_attachments = []
    }

sendHelpMessageMsg :: VkMessage
sendHelpMessageMsg = defaultMessage { _VM_text = Just $ helpCommand defStateGen }


{-
sendHelpMessageUpd = VkUpdate {
    _VU_value = String "help request update"
    , _VU_object = VEMsg sendHelpMessageMsg
    }
-}
sendHelpMessageUpd = buildUpdate (VEMsg sendHelpMessageMsg) "help request update"

successReply :: VkReply
successReply = VkReply Nothing


buildUpdate :: VkEvent -> T.Text -> VkUpdate
buildUpdate event msg = VkUpdate {
    _VU_value = String msg
    , _VU_object = event
    }


sendKeyboardMessage = defaultMessage { _VM_text = Just $ setRepNumCommand defStateGen }

sendKeyboardMessageUpd :: VkUpdate
sendKeyboardMessageUpd = buildUpdate (VEMsg sendKeyboardMessage) "send repnum buttons request"

{-
[{"object":{"client_info":{"lang_id":0,"button_actions":["text","vkpay","ope
        n_app","location","open_link","callback","intent_subscribe","intent_unsubscr
        ibe"],"keyboard":true,"carousel":true,"inline_keyboard":true},"message":{"at
        tachments":[],"text":"1","peer_id":22757746,"conversation_message_id":1383,"
        payload":"{\"payload\":\"set 1\"}","random_id":0,"date":1634665118,"from_id"
        :22757746,"is_hidden":false,"fwd_messages":[],"id":1450,"important":false,"o
        ut":0}},"group_id":199493573,"type":"message_new","event_id":"20a812cb0e42e8
        9e4fbe5f49df1600e1e79983ac"}]
-}

setRepNumCallback :: Int -> VkMyCallback
setRepNumCallback int = VkMyCallback {
    _VMC_from_id = defaultUser
    , _VMC_text = Just $ S.showTL int
    , _VMC_payload = VkPayload ("set " <> S.showTL int)
    
    }

setRepNumUpdate :: Int -> VkUpdate
setRepNumUpdate repnum = buildUpdate (VECallback $ setRepNumCallback repnum) "set repeat number update"


simpleMessage = defaultMessage { _VM_text = Just "hello, this is yoba" }

simpleMessageUpdate :: VkUpdate
simpleMessageUpdate = buildUpdate (VEMsg simpleMessage) "simple message to echo"


