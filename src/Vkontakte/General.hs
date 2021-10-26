{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

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
      { configUrl :: T.Text
      , configAccessToken :: T.Text
      , configGroupID :: Integer
      , configApiVersion :: T.Text
      }
    deriving stock (Show, Eq, Generic)
    deriving anyclass PrettyShow

data VkConfigDefault =
   VkConfDef
      { defaultconfUrl :: T.Text
      , defaultconfApiVersion :: T.Text
      }
    deriving stock (Show, Eq, Generic)
    deriving anyclass PrettyShow


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
    deriving stock (Show, Eq, Generic)
    deriving anyclass PrettyShow

data VkStateMut =
   VKSM
      { vkTs :: T.Text -- timestamp
      , vkRndGen :: StdGen
      }
    deriving stock (Show)
