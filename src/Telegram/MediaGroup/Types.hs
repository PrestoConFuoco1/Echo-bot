{-# LANGUAGE
    DeriveGeneric
    #-}
module Telegram.MediaGroup.Types where

import GHC.Generics
import Telegram.Entity
import qualified Data.Text as T
import qualified GenericPretty as GP
import Data.Aeson.Types
import Telegram.ProcessMessage.Types

data TlMediaGroupIdentifier = TlMediaGroupIdentifier {
    _TMGI_chat :: TlChat,
    _TMGI_user :: Maybe TlUser,
    _TMGI_mediaGroupID :: T.Text
    } deriving (Show, Eq, Ord, Generic)

instance GP.PrettyShow TlMediaGroupIdentifier


data TlInputMediaPhoto = TlInputMediaPhoto {
    _TIMP_caption :: Maybe T.Text
    , _TIMP_media :: T.Text
    -- here it is file_id that exists on the Telegram servers
    } deriving (Show, Generic)

instance ToJSON TlInputMediaPhoto where
    toJSON ph = object [
        "type" .= ("photo" :: T.Text),
        "media" .= _TIMP_media ph,
        "caption" .= _TIMP_caption ph
        ]


data TlMediaGroupPair = TlMediaGroupPair {
    _TMGP_identifier :: TlMediaGroupIdentifier,
    _TMGP_items :: [TlPhotoSize]
    } deriving (Show, Generic)
instance GP.PrettyShow TlMediaGroupPair


