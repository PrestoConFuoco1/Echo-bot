{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vkontakte.Entity where

import Data.Aeson.Types
import qualified Data.Text as T (Text)
import GHC.Generics (Generic)
import GenericPretty
import Vkontakte.Attachment
import Data.Function (on)
import DerivingJSON

data VkMessage =
   VkMessage
      { messageID :: Integer
      , messageFromID :: VkUser
      , messageText :: Maybe T.Text
      , messageAttachments :: [VkAttachment]
      }
    deriving stock (Eq, Show, Generic)
    deriving anyclass PrettyShow

instance FromJSON VkMessage where
    parseJSON val = fixText . unBotSelectorModifier <$> parseJSON val
     where
       fixText m =
          if messageText m == Just ""
             then m {messageText = Nothing}
             else m

data VkChat = VkChat

newtype VkUser =
   VkUser
      { userID :: Integer
      }
    deriving stock (Show, Generic)
    deriving newtype FromJSON
    deriving anyclass PrettyShow
   
instance Eq VkUser where
   (==) = (==) `on` userID

instance Ord VkUser where
   compare = compare `on` userID

data VkMyCallback =
   VkMyCallback
      { mycallbackFromID :: VkUser
      , mycallbackText :: Maybe T.Text
      , mycallbackPayload :: VkPayload
      }
    deriving stock (Show, Eq, Generic)
    deriving (FromJSON) via BotSelectorModifier VkMyCallback

newtype VkPayload =
   VkPayload
      { payloadPayload :: T.Text
      }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via BotSelectorModifier VkPayload

