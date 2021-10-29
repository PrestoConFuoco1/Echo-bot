module Test.Vkontakte.TestData where

import Environment
import Test.Hspec
import Data.IORef
import Handlers
import qualified HTTP.Types as H
import App.Handle as D
import Vkontakte
import BotTypesClass.ClassTypes
import BotTypesClass.VkInstance
import Execute.Vkontakte
import Execute
import Execute.Logic
import Data.Aeson
import Data.Text as T
import qualified Stuff as S


{-
data VkMessage = VkMessage {
    messageID :: Integer,
    messageFromID :: VkUser,
    messageText :: Maybe T.Text,
    messageAttachments :: [VkAttachment]
    --_VM_payload :: Maybe VkMyCallback
    } deriving (Eq, Show, Generic)


-}

defaultUser = VkUser { userID = 0 }

defaultMessage = VkMessage {
    messageID = 0
    , messageFromID = defaultUser
    , messageText = Nothing
    , messageAttachments = []
    }

sendHelpMessageMsg :: VkMessage
sendHelpMessageMsg = defaultMessage { messageText = Just $ getHelpCommand defStateGen }


{-
sendHelpMessageUpd = VkUpdate {
    updateValue = String "help request update"
    , updateObject = VEMsg sendHelpMessageMsg
    }
-}
sendHelpMessageUpd = buildUpdate (VEMsg sendHelpMessageMsg) "help request update"

successReply :: VkReply
successReply = VkReply Nothing (String "success reply")


buildUpdate :: VkEvent -> T.Text -> VkUpdate
buildUpdate event msg = VkUpdate {
    updateValue = String msg
    , updateObject = event
    }


sendKeyboardMessage = defaultMessage { messageText = Just $ getSetRepNumCommand defStateGen }

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
    mycallbackFromID = defaultUser
    , mycallbackText = Just $ S.showT int
    , mycallbackPayload = VkPayload ("set " <> S.showT int)
    
    }

setRepNumUpdate :: Int -> VkUpdate
setRepNumUpdate repnum = buildUpdate (VECallback $ setRepNumCallback repnum) "set repeat number update"


simpleMessage = defaultMessage { messageText = Just "hello, this is yoba" }

simpleMessageUpdate :: VkUpdate
simpleMessageUpdate = buildUpdate (VEMsg simpleMessage) "simple message to echo"


