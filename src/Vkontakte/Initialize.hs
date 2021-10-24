{-# LANGUAGE DeriveGeneric #-}
module Vkontakte.Initialize where

import Data.Aeson (decode)
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BSL (ByteString)
import qualified Data.Text as T (Text)
import GHC.Generics (Generic)
import GenericPretty

data VkInitData =
   VkInitData
      { _VID_key :: T.Text
      , _VID_server :: T.Text
      , _VID_timestamp :: T.Text
      }
   deriving (Show, Generic)

instance PrettyShow VkInitData

parseInitResp :: BSL.ByteString -> Either String VkInitData
parseInitResp = eithParsed
  where
    parseInitRep =
       withObject "object: key, server, ts" $ \o' -- Parser a
        -> do
          o <- o' .: "response"
          key <- o .: "key" :: Parser T.Text
          server <- o .: "server" :: Parser T.Text
          ts <- o .: "ts" :: Parser T.Text
          pure
             VkInitData
                { _VID_key = key
                , _VID_server = server
                , _VID_timestamp = ts
                }
    initReplyToJSON =
       maybe
          (Left "Couldn't parse getLongPollServer reply")
          Right .
       decode
    eithParsed x =
       initReplyToJSON x >>= parseEither parseInitRep
