{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Vkontakte.Update where

import Data.Aeson (decode)
import Data.Aeson.Types
import Data.Foldable (asum)
import qualified Data.Text as T (Text)
import qualified Data.Text.Encoding as E (encodeUtf8)
import GHC.Generics
import GenericPretty
import qualified Stuff as S (showT)
import Types
import Vkontakte.Entity
import qualified Data.ByteString.Lazy as BS (fromStrict)

data VkReply =
   VkReply
      { replyFailed :: Maybe Int
      , replyVal :: Value
      }
    deriving stock (Eq, Show, Generic)
    deriving anyclass PrettyShow

instance FromJSON VkReply where
   parseJSON x =
      ($ x) $
      withObject "Vkontakte reply object" $ \o -> do
         failed <- o .:? "failed"
         pure $ VkReply failed x

data VkUpdateReplySuccess =
   VkUpdateReplySuccess
      { replysuccessUpdates :: Value
      , replysuccessTs :: Maybe T.Text
      }
    deriving stock (Show, Eq, Generic)
    deriving anyclass PrettyShow

data VkUpdateReplyError =
   VkUpdateReplyError
      { replyerrorFailed :: Int
      , replyerrorTs :: Maybe T.Text
      }
    deriving stock (Show, Eq, Generic)
    deriving anyclass PrettyShow

parseUpdatesResponse2 ::
      Value
   -> Either String (UpdateResponse VkUpdateReplySuccess VkUpdateReplyError)
parseUpdatesResponse2 =
   parseEither $
   withObject "Vkontakte update object" $ \o -> do
      ts <-
         asum
            [ o .:? "ts"
            , fmap
                 (fmap S.showT)
                 (o .:? "ts" :: Parser (Maybe Integer))
            ]
      let success = do
             updates <- o .: "updates"
             pure $
                VkUpdateReplySuccess
                   {replysuccessUpdates = updates, replysuccessTs = ts}
          err = do
             failed <- o .: "failed"
             pure $
                VkUpdateReplyError
                   {replyerrorFailed = failed, replyerrorTs = ts}
      asum
         [fmap UpdateResponse success, fmap UpdateError err]

data VkUpdate =
   VkUpdate
      { updateValue :: Value
      , updateObject :: VkEvent
      }
    deriving stock (Eq, Show)

data VkEvent
   = VEMsg VkMessage
   | VECallback VkMyCallback
   | VEUnexpectedEvent
    deriving stock (Eq, Show)

instance FromJSON VkUpdate where
   parseJSON value =
      ($value) $
      withObject "update object" $ \o -> do
         updType <- o .: "type" :: Parser T.Text
         event <-
            case updType of
               "message_new" -> do
                  q <- o .: "object"
                  msg <- q .: "message" :: Parser Value
                  asum
                     [ VECallback <$> parseCallback msg
                     , VEMsg <$> parseJSON msg
                     ]
               _ -> pure VEUnexpectedEvent
         pure $
            VkUpdate
               value
               event

parseCallback :: Value -> Parser VkMyCallback
parseCallback =
   withObject "Expected message object with payload" $ \msg -> do
      pt <- msg .: "payload" :: Parser T.Text
      let pbs = E.encodeUtf8 pt
          pVal = decode $ BS.fromStrict pbs :: Maybe Value
      payload <-
         maybe
            (fail "unable to parse payload object")
            pure
            pVal >>=
         parseJSON
      text <- msg .: "text"
      from_id <- msg .: "from_id"
      pure $ VkMyCallback from_id text payload
