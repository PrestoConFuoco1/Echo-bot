{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Vkontakte.General where

import qualified Data.Text as T (Text)
import GHC.Generics (Generic)
import GenericPretty (PrettyShow)
import HTTPTypes as H
import System.Random (StdGen)

defaultVkParams :: VkStateConst -> H.ParamsList
defaultVkParams sc =
  defaultVkParams' (vkAccessToken sc) (apiVersion sc)

defaultVkParams' :: T.Text -> T.Text -> H.ParamsList
defaultVkParams' accTok apiV =
  [unit "access_token" accTok, unit "v" apiV]

vkTakesJSON :: Bool
vkTakesJSON = False

data VkStateConst = VKSC
  { vkKey :: T.Text,
    vkServer :: T.Text,
    vkUrl :: T.Text, -- only for methods
    vkAccessToken :: T.Text,
    vkGroupID :: Integer,
    apiVersion :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyShow)

data VkStateMut = VKSM
  { vkTs :: T.Text, -- timestamp
    vkRndGen :: StdGen
  }
  deriving stock (Show)
