{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Config.Types (VkConfig(..), TlConfig(..)) where

import qualified Data.Text as T (Text)
import GHC.Generics (Generic)
import GenericPretty (PrettyShow)


data TlConfig = TlConf
  { tlConfigUpdateID :: Integer,
    tlConfigUrl :: T.Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (PrettyShow)

data VkConfig = VkConf
  { vkConfigUrl :: T.Text,
    vkConfigAccessToken :: T.Text,
    vkConfigGroupID :: Integer,
    vkConfigApiVersion :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyShow)

