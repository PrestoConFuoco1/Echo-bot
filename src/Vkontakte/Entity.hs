{-# LANGUAGE
    DeriveGeneric
    #-}
module Vkontakte.Entity where

import Data.Aeson.Types
import GHC.Generics (Generic)
import qualified Data.Text.Lazy as TL (Text)
import qualified Data.Text as T (Text)
import GenericPretty
import Vkontakte.Attachment


-- Возможный вариант: любое "message_new" при наличии payload интерпретировать
-- как callback. 
-- Другой - изврат. (кажется)
--------------------------------------------------

data VkMessage = VkMessage {
    _VM_id :: Integer,
    _VM_from_id :: VkUser,
    _VM_text :: Maybe T.Text,
    _VM_attachments :: [VkAttachment]
    --_VM_payload :: Maybe VkMyCallback
    } deriving (Eq, Show, Generic)

instance FromJSON VkMessage where
    parseJSON = fmap fixText . genericParseJSON defaultOptions {
        fieldLabelModifier = drop 4 }
      where fixText m = if _VM_text m == Just ""
                        then m {_VM_text = Nothing}
                        else m

-----------------------------------------------------

data VkChat = VkChat

-----------------------------------------------------

data VkUser = VkUser {
    _VU_id :: Integer
    } deriving (Show, Generic)


instance ToJSON VkUser where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }

instance FromJSON VkUser where
    parseJSON x = VkUser <$> parseJSON x

instance Eq VkUser where
    (==) u1 u2 = (==) (_VU_id u1) (_VU_id u2)

instance Ord VkUser where
    compare u1 u2 = compare (_VU_id u1) (_VU_id u2)



data VkMyCallback = VkMyCallback {
    _VMC_from_id :: VkUser,
    _VMC_text    :: Maybe TL.Text,
    _VMC_payload :: VkPayload
    } deriving (Show, Eq, Generic)


instance ToJSON VkMyCallback where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 5 }


instance FromJSON VkMyCallback where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }


data VkPayload = VkPayload {
    _VP_payload :: TL.Text
    } deriving (Show, Eq, Generic)


instance ToJSON VkPayload where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }


instance FromJSON VkPayload where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 4 }

instance PrettyShow VkUser
instance PrettyShow VkMessage

