module Handlers where

import qualified App.Logger as L
import App.Handle as D
import qualified App.Handle.Telegram as T
import qualified App.Handle.Vkontakte as V
import Prelude hiding (log)
import BotClass.ClassTypes
import Telegram
import Vkontakte
import Data.IORef
import Types


defaultHandle :: (BotClassTypes s) => s -> Handle s IO
defaultHandle s = Handle {
    log = L.emptyLogger,

    --D.commonEnv = error "commonEnv should not be used in this test",
    D.commonEnv = defStateGen,

    insertUser = error "insertUser should not be used in this test",
    getUser = error "getUser should not be used in this test",

    getConstState = error "getConstState should not be used in this test",

    D.getUpdates = error "getUpdates should not be used in this test",
    D.sendEcho = error "sendEcho should not be used in this test",
    D.sendHelp = error "sendHelp should not be used in this test",
    D.sendKeyboard = error "sendKeyboard should not be used in this test",
    D.sendRepNumMessage = error "sendRepNumMessage should not be used in this test",

    specH = error "special handle should not be used in this test"
    }


defaultTelegramHandle :: TlHandler IO
defaultTelegramHandle = TlHandler {
    getUpdateID = error "getUpdateID should not be used in this test",
    putUpdateID = error "putUpdateID should not be used in this test",
    insertMediaGroupPhoto = error "insertMediaGroupPhoto should not be used in this test",
    purgeMediaGroups = error "purgeMediaGroups should not be used in this test",
    getMediaGroups = error "getMediaGroups should not be used in this test"
    }

defaultVkHandle :: VkHandler IO
defaultVkHandle = VkHandler {
    getRandomID = error "getRandomID should not be used in this test",
    getTimestamp = error "getTimestamp should not be used in this test",
    putTimestamp = error "putTimestamp should not be used in this test"
    }

   

