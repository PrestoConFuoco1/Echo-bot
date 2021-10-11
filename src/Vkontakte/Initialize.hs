{-# LANGUAGE
    DeriveGeneric
    #-}
module Vkontakte.Initialize where

import Data.Aeson (decode)
import Data.Aeson.Types
import GHC.Generics (Generic)
import System.Random (StdGen, randomR)
import qualified Data.Text.Lazy as TL (Text, unpack, pack, toStrict)
import qualified Data.Text as T (Text, unpack, pack)
import qualified Data.ByteString.Lazy as BSL (ByteString)

import GenericPretty

import Vkontakte.Entity as VE
import Vkontakte.Attachment as VA
import Vkontakte.Keyboard as VKb
import HTTPRequests as H


data VkInitData = VkInitData {
    _VID_key :: TL.Text
    , _VID_server :: TL.Text
    , _VID_timestamp :: TL.Text
    } deriving (Show, Generic)
instance PrettyShow VkInitData

--parseInitResp :: BSL.ByteString -> Either String (TL.Text, TL.Text, TL.Text)
parseInitResp :: BSL.ByteString -> Either String VkInitData
parseInitResp = eithParsed
  where parseInitRep = withObject "object: key, server, ts" $ \o' -> do -- Parser a
            o <- o' .: "response"
            key <- o .: "key" :: Parser TL.Text
            server <- o .: "server" :: Parser TL.Text
            ts <- o .: "ts" :: Parser TL.Text
            return VkInitData {
                _VID_key = key
                , _VID_server = server
                , _VID_timestamp = ts
                }
        initReplyToJSON =
                maybe (Left "Couldn't parse getLongPollServer reply") Right
                . decode
        eithParsed x = return x >>= initReplyToJSON >>= parseEither parseInitRep


