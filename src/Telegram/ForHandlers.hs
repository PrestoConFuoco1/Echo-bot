{-# LANGUAGE
    OverloadedStrings
    , DeriveGeneric
    , RecordWildCards
    #-}
module Telegram.ForHandlers where


import HTTPRequests as H
import Data.Aeson (encode)
import Data.Aeson.Types
import GHC.Generics (Generic)
import Data.Foldable (asum)
import qualified Stuff as S (safeHead)
import qualified Data.Text.Lazy.Encoding as EL (decodeUtf8)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as TL (Text, unpack, pack)
import qualified Data.Text as T (Text, unpack, pack)

import GenericPretty as GP
import Telegram.ProcessMessage.Types
import Telegram.MediaGroup.Types
import qualified Data.Map as M


data Tele = Tele
dummyTl = Tele

tlTakesJSON = True

data TlConfig = TlConf {
        _TC_updID :: Integer,
        _TC_url :: TL.Text
    } deriving (Show, Generic)
instance PrettyShow TlConfig

-----------------------------------------------------------

data TlStateConst = TLSC {
    tlUrl :: TL.Text
    } deriving (Show)

data TlStateMut = TLSM {
    tlUpdateID :: Integer,
    photoMediaGroups :: M.Map TlMediaGroupIdentifier [TlPhotoSize]
    } deriving (Show)

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


data TlMediaGroupPair = TlMediaGroupPair {
    _TMGP_identifier :: TlMediaGroupIdentifier,
    _TMGP_items :: [TlPhotoSize]
    } deriving (Show, Generic)
instance GP.PrettyShow TlMediaGroupPair

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



