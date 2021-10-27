{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vkontakte.Entity where

import qualified Data.Aeson.Types as AeT
import Data.Function (on)
import qualified Data.Text as T (Text)
import DerivingJSON (BotSelectorModifier (..))
import GHC.Generics (Generic)
import GenericPretty (PrettyShow)
import Vkontakte.Attachment (VkAttachment)

data VkMessage = VkMessage
  { messageID :: Integer,
    messageFromID :: VkUser,
    messageText :: Maybe T.Text,
    messageAttachments :: [VkAttachment]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (PrettyShow)

instance AeT.FromJSON VkMessage where
  parseJSON val = fixText . unBotSelectorModifier <$> AeT.parseJSON val
    where
      fixText m =
        if messageText m == Just ""
          then m {messageText = Nothing}
          else m

data VkChat = VkChat

newtype VkUser = VkUser
  { userID :: Integer
  }
  deriving stock (Show, Generic)
  deriving newtype (AeT.FromJSON)
  deriving anyclass (PrettyShow)

instance Eq VkUser where
  (==) = (==) `on` userID

instance Ord VkUser where
  compare = compare `on` userID

data VkMyCallback = VkMyCallback
  { mycallbackFromID :: VkUser,
    mycallbackText :: Maybe T.Text,
    mycallbackPayload :: VkPayload
  }
  deriving stock (Show, Eq, Generic)
  deriving (AeT.FromJSON) via BotSelectorModifier VkMyCallback

newtype VkPayload = VkPayload
  { payloadPayload :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (AeT.ToJSON, AeT.FromJSON) via BotSelectorModifier VkPayload
