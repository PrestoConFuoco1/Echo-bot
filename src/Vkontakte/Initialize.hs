{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Vkontakte.Initialize where

import Data.Aeson (decode)
import Data.Aeson.Types ((.:))
import qualified Data.Aeson.Types as AeT
import qualified Data.ByteString.Lazy as BSL (ByteString)
import qualified Data.Text as T (Text)
import GHC.Generics (Generic)
import GenericPretty

data VkInitData = VkInitData
  { initKey :: T.Text,
    initServer :: T.Text,
    initTimestamp :: T.Text
  }
  deriving (Show, Generic)
  deriving anyclass (PrettyShow)

parseInitResp :: BSL.ByteString -> Either String VkInitData
parseInitResp = eithParsed
  where
    parseInitRep =
      AeT.withObject "object: key, server, ts" $ \o' -> -- AeT.Parser a
        do
          o <- o' .: "response"
          key <- o .: "key" :: AeT.Parser T.Text
          server <- o .: "server" :: AeT.Parser T.Text
          ts <- o .: "ts" :: AeT.Parser T.Text
          pure
            VkInitData
              { initKey = key,
                initServer = server,
                initTimestamp = ts
              }
    initReplyToJSON =
      maybe
        (Left "Couldn't parse getLongPollServer reply")
        Right
        . decode
    eithParsed x =
      initReplyToJSON x >>= AeT.parseEither parseInitRep
