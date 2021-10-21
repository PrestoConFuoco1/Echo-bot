{-# LANGUAGE
    DeriveGeneric
    #-}
module Vkontakte.General where

import GHC.Generics (Generic)
import System.Random (StdGen)
import qualified Data.Text.Lazy as TL (Text)
import qualified Data.Text as T (Text)
import GenericPretty
import HTTPRequests as H

defaultVkParams :: VkStateConst -> H.ParamsList
defaultVkParams sc = defaultVkParams' (vkAccessToken sc) (apiVersion sc)

defaultVkParams' :: TL.Text -> T.Text -> H.ParamsList
defaultVkParams' accTok apiV =
    [unit "access_token" accTok,
     unit "v" apiV ]


vkTakesJSON :: Bool
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


