{-# LANGUAGE
    OverloadedStrings
    , DeriveGeneric
    , RecordWildCards
    #-}
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

insertMediaGroupPhoto' :: TlMediaGroupIdentifier -> TlPhotoSize -> TlStateMut -> TlStateMut
insertMediaGroupPhoto' key value sm =
    let newMap = M.insertWith (++) key [value] $ photoMediaGroups sm
-- what is first and what is second argument in (++) here?
    in  sm { photoMediaGroups = newMap }

purgeMediaGroups' :: TlStateMut -> TlStateMut
purgeMediaGroups' sm = sm { photoMediaGroups = M.empty }

getMediaGroups' :: TlStateMut -> [TlMediaGroupPair]
getMediaGroups' TLSM { photoMediaGroups = x } = map f $ M.toList x
  where f (k, v) = TlMediaGroupPair k v


data TlHandler m = TlHandler {
    getUpdateID :: m Integer,
    putUpdateID :: Integer -> m (),
    insertMediaGroupPhoto :: TlMediaGroupIdentifier -> TlPhotoSize -> m (),
    purgeMediaGroups :: m (),
    getMediaGroups :: m [TlMediaGroupPair]
    }



