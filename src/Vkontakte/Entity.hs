{-# LANGUAGE
    DeriveGeneric,
    RecordWildCards
    #-}
module Vkontakte.Entity where

import Data.Aeson (encode, decode)
import Data.Aeson.Types
import GHC.Generics (Generic)
import Data.Foldable (asum)
import System.Random (StdGen)
import qualified Stuff as S (showTL)
import qualified Data.Text.Lazy.Encoding as EL (decodeUtf8, encodeUtf8)
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL (Text, unpack, pack, toStrict)
import qualified Data.Text as T (Text, unpack, pack)
import qualified Data.ByteString.Lazy as BSL (ByteString)
import Control.Applicative ((<|>))

import GenericPretty

import qualified HTTPRequests as H

import Vkontakte.Attachment
import Types


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

{-instance ToJSON VkMessage where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }
-}
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
-- Eq? Maybe two users are equal if their id is the same?


instance ToJSON VkUser where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }

instance FromJSON VkUser where
    parseJSON x = fmap VkUser $ parseJSON x

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

