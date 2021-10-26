{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}

module Vkontakte.Update where

import Data.Aeson (decode)
import qualified Data.Aeson.Types as AeT
import Data.Aeson.Types ((.:), (.:?))
import Data.Foldable (asum)
import qualified Data.Text as T (Text)
import qualified Data.Text.Encoding as E (encodeUtf8)
import GHC.Generics
import GenericPretty
import qualified Stuff as S (showT)
import qualified Types as Y
import Vkontakte.Entity
import qualified Data.ByteString.Lazy as BS (fromStrict)

data VkReply =
   VkReply
      { replyFailed :: Maybe Int
      , replyVal :: AeT.Value
      }
    deriving stock (Eq, Show, Generic)
    deriving anyclass PrettyShow

instance AeT.FromJSON VkReply where
   parseJSON x =
      ($ x) $
      AeT.withObject "Y.Vkontakte reply object" $ \o -> do
         failed <- o .:? "failed"
         pure $ VkReply failed x

data VkUpdateReplySuccess =
   VkUpdateReplySuccess
      { replysuccessUpdates :: AeT.Value
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
      AeT.Value
   -> Either String (Y.UpdateResponse VkUpdateReplySuccess VkUpdateReplyError)
parseUpdatesResponse2 =
   AeT.parseEither $
   AeT.withObject "Y.Vkontakte update object" $ \o -> do
      ts <-
         asum
            [ o .:? "ts"
            , fmap
                 (fmap S.showT)
                 (o .:? "ts" :: AeT.Parser (Maybe Integer))
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
         [fmap Y.UpdateResponse success, fmap Y.UpdateError err]

data VkUpdate =
   VkUpdate
      { updateValue :: AeT.Value
      , updateObject :: VkEvent
      }
    deriving stock (Eq, Show)

data VkEvent
   = VEMsg VkMessage
   | VECallback VkMyCallback
   | VEUnexpectedEvent
    deriving stock (Eq, Show)

instance AeT.FromJSON VkUpdate where
   parseJSON value =
      ($value) $
      AeT.withObject "update object" $ \o -> do
         updType <- o .: "type" :: AeT.Parser T.Text
         event <-
            case updType of
               "message_new" -> do
                  q <- o .: "object"
                  msg <- q .: "message" :: AeT.Parser AeT.Value
                  asum
                     [ VECallback <$> parseCallback msg
                     , VEMsg <$> AeT.parseJSON msg
                     ]
               _ -> pure VEUnexpectedEvent
         pure $
            VkUpdate
               value
               event

parseCallback :: AeT.Value -> AeT.Parser VkMyCallback
parseCallback =
   AeT.withObject "Expected message object with payload" $ \msg -> do
      pt <- msg .: "payload" :: AeT.Parser T.Text
      let pbs = E.encodeUtf8 pt
          pVal = decode $ BS.fromStrict pbs :: Maybe AeT.Value
      payload <-
         maybe
            (fail "unable to parse payload object")
            pure
            pVal >>=
         AeT.parseJSON
      text <- msg .: "text"
      from_id <- msg .: "from_id"
      pure $ VkMyCallback from_id text payload
