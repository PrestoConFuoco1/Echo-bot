{-# LANGUAGE
    DeriveGeneric,
    RecordWildCards
    #-}

module Vkontakte.Types (module Vkontakte.Types, module VE, module VA, module VKb) where


import Data.Aeson (decode)
import Data.Aeson.Types
import GHC.Generics (Generic)
import System.Random (StdGen, randomR)
import qualified Data.Text.Lazy as TL (Text, unpack, pack, toStrict)
import qualified Data.Text as T (Text, unpack, pack)
import qualified Data.ByteString.Lazy as BSL (ByteString)

import GenericPretty

import Vkontakte.Entity as VE
import Vkontakte.Attachment as VA
import Vkontakte.Keyboard as VKb
import HTTPRequests as H


defaultVkParams sc = defaultVkParams' (vkAccessToken sc) (apiVersion sc)

defaultVkParams' accTok apiV =
    [unit "access_token" accTok,
     unit "v" apiV ]




data Vk = Vk
dummyVk = Vk

vkTakesJSON = False

----------------------------------------------

data VkConfig = VkConf {
    _VC_vkUrl :: TL.Text,
    _VC_accessToken :: TL.Text,
    _VC_groupID :: Integer,
    _VC_apiV :: T.Text
    } deriving (Show, Eq, Generic)

instance PrettyShow VkConfig

data VkConfigDefault = VkConfDef {
    _VCD_vkUrl :: TL.Text,
    _VCD_apiV :: T.Text
    
    } deriving (Show, Eq, Generic)

instance PrettyShow VkConfigDefault

defaultVkConfig :: VkConfigDefault
defaultVkConfig = VkConfDef "https://api.vk.com/method/" "5.124"
---------------------------------------------

data VkStateConst = VKSC {
    vkKey :: TL.Text,
    vkServer :: TL.Text,
    vkUrl :: TL.Text, -- only for methods
    vkAccessToken :: TL.Text,
    vkGroupID :: Integer,
    apiVersion :: T.Text
    } deriving (Show, Eq)

data VkStateMut = VKSM {
    vkTs :: TL.Text, -- timestamp
    vkRndGen :: StdGen
    } deriving (Show)

getRandomID' :: VkStateMut -> (VkStateMut, Integer)
getRandomID' sm =
    let (rndInt32, g') = randomR (0::Integer, 2^32-1) $ vkRndGen sm
    in  (sm { vkRndGen = g' }, rndInt32)

putTimestamp' :: TL.Text -> VkStateMut -> VkStateMut
putTimestamp' newTs sm = sm { vkTs = newTs }

getTimestamp' :: VkStateMut -> TL.Text
getTimestamp' = vkTs

data VkHandler m = VkHandler {
    getRandomID :: m Integer,
    getTimestamp :: m TL.Text,
    putTimestamp :: TL.Text -> m ()
    }

--------------------------------------------



