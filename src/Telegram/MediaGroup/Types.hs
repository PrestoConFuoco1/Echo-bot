{-# LANGUAGE
    DeriveGeneric
    #-}
module Telegram.MediaGroup.Types where

import GHC.Generics
import Telegram.Entity
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified GenericPretty as GP

import Data.Aeson.Types


data TlMediaGroupIdentifier = TlMediaGroupIdentifier {
    _TMGI_chat :: TlChat,
    _TMGI_user :: Maybe TlUser,
    _TMGI_mediaGroupID :: T.Text
    } deriving (Show, Eq, Ord, Generic)

instance GP.PrettyShow TlMediaGroupIdentifier

data TlInputMediaPhoto = TlInputMediaPhoto {
    _TIMP_media :: T.Text
    -- here it is file_id that exists on the Telegram servers
    } deriving (Show, Generic)

instance ToJSON TlInputMediaPhoto where
    toJSON ph = object ["type" .= ("photo" :: TL.Text), "media" .= _TIMP_media ph]


