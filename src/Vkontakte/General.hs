{-# LANGUAGE DeriveGeneric #-}

module Vkontakte.General where

import qualified Data.Text as T (Text)
import GHC.Generics (Generic)
import GenericPretty
import HTTPRequests as H
import System.Random (StdGen)

defaultVkParams :: VkStateConst -> H.ParamsList
defaultVkParams sc =
   defaultVkParams' (vkAccessToken sc) (apiVersion sc)

defaultVkParams' :: T.Text -> T.Text -> H.ParamsList
defaultVkParams' accTok apiV =
   [unit "access_token" accTok, unit "v" apiV]

vkTakesJSON :: Bool
vkTakesJSON = False

data VkConfig =
   VkConf
      { _VC_vkUrl :: T.Text
      , _VC_accessToken :: T.Text
      , _VC_groupID :: Integer
      , _VC_apiV :: T.Text
      }
   deriving (Show, Eq, Generic)

instance PrettyShow VkConfig

data VkConfigDefault =
   VkConfDef
      { _VCD_vkUrl :: T.Text
      , _VCD_apiV :: T.Text
      }
   deriving (Show, Eq, Generic)

instance PrettyShow VkConfigDefault

defaultVkConfig :: VkConfigDefault
defaultVkConfig =
   VkConfDef "https://api.vk.com/method/" "5.124"

data VkStateConst =
   VKSC
      { vkKey :: T.Text
      , vkServer :: T.Text
      , vkUrl :: T.Text -- only for methods
      , vkAccessToken :: T.Text
      , vkGroupID :: Integer
      , apiVersion :: T.Text
      }
   deriving (Show, Eq)

data VkStateMut =
   VKSM
      { vkTs :: T.Text -- timestamp
      , vkRndGen :: StdGen
      }
   deriving (Show)
