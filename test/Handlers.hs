module Handlers where

import qualified App.Logger as L
import App.Handle as D
import qualified App.Handle.Telegram as T
import qualified App.Handle.Vkontakte as V
import Prelude hiding (log)
import BotTypesClass.ClassTypes
import Telegram
import Vkontakte
import Data.IORef
import qualified Environment as Env

defaultHandle :: (BotClassTypes s) => D.BotHandler s IO
defaultHandle = D.BotHandler {
    log = L.emptyLogger,

    --D.commonEnv = error "commonEnv should not be used in this test",
    D.commonEnv = Env.defStateGen,

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


defaultTelegramHandle :: T.TlHandler IO
defaultTelegramHandle = T.TlHandler {
    T.getUpdateID = error "getUpdateID should not be used in this test",
    T.putUpdateID = error "putUpdateID should not be used in this test",
    T.insertMediaGroupUnit = error "insertMediaGroupPhoto should not be used in this test",
    T.purgeMediaGroups = error "purgeMediaGroups should not be used in this test",
    T.getMediaGroups = error "getMediaGroups should not be used in this test"
    }

defaultVkHandle :: V.VkHandler IO
defaultVkHandle = V.VkHandler {
    V.getRandomID = error "getRandomID should not be used in this test",
    V.getTimestamp = error "getTimestamp should not be used in this test",
    V.putTimestamp = error "putTimestamp should not be used in this test"
    }

   

