module Telegram.ForHandlers where


import Telegram.ProcessMessage.Types
import Telegram.MediaGroup.Types
import qualified Data.Map as M
import Telegram.General

-----------------------------------------------------------
getUpdateID' :: TlStateMut -> Integer
getUpdateID' x = tlUpdateID x

putUpdateID' :: Integer -> TlStateMut -> TlStateMut
putUpdateID' uid m = m { tlUpdateID = uid }

insertMediaGroupUnit' :: TlMediaGroupIdentifier -> TlMediaGroupUnit -> TlStateMut -> TlStateMut
insertMediaGroupUnit' key value sm =
    let newMap = M.insertWith (++) key [value] $ mediaGroups sm
    in  sm { mediaGroups = newMap }



purgeMediaGroups' :: TlStateMut -> TlStateMut
purgeMediaGroups' sm = sm { mediaGroups = M.empty }

getMediaGroups' :: TlStateMut -> [TlMediaGroupPair]
getMediaGroups' TLSM { mediaGroups = x } = map f $ M.toList x
  where f (k, v) = TlMediaGroupPair k v


data TlHandler m = TlHandler {
    getUpdateID :: m Integer,
    putUpdateID :: Integer -> m (),
    insertMediaGroupUnit :: TlMediaGroupIdentifier -> TlMediaGroupUnit -> m (),
    purgeMediaGroups :: m (),
    getMediaGroups :: m [TlMediaGroupPair]
    }



