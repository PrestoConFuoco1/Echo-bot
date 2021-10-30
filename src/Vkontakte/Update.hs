{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Vkontakte.Update
    ( VkReply(..)
    , VkUpdate(..)
    , VkUpdateReplyError(..)
    , VkUpdateReplySuccess(..)
    , parseUpdatesResponse2
    , VkEvent(..)
    ) where

import Data.Aeson (decode)
import Data.Aeson.Types ((.:), (.:?))
import qualified Data.Aeson.Types as AeT
import qualified Data.ByteString.Lazy as BS (fromStrict)
import Data.Foldable (asum)
import qualified Data.Text as T (Text)
import qualified Data.Text.Encoding as E (encodeUtf8)
import GHC.Generics
import GenericPretty
import qualified Stuff as S (showT)
import Vkontakte.Entity

data VkReply =
    VkReply
        { replyFailed :: Maybe Int
        , replyVal :: AeT.Value
        }
  deriving  (Eq, Show, Generic)
  deriving anyclass (PrettyShow)

instance AeT.FromJSON VkReply where
    parseJSON x =
        ($ x) $
        AeT.withObject "M.Vkontakte reply object" $ \o -> do
            failed <- o .:? "failed"
            pure $ VkReply failed x

data VkUpdateReplySuccess =
    VkUpdateReplySuccess
        { replysuccessUpdates :: AeT.Value
        , replysuccessTs :: Maybe T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)

data VkUpdateReplyError =
    VkUpdateReplyError
        { replyerrorFailed :: Int
        , replyerrorTs :: Maybe T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)

parseUpdatesResponse2 ::
       AeT.Value
    -> Either String (Either VkUpdateReplyError VkUpdateReplySuccess)
parseUpdatesResponse2 =
    AeT.parseEither $
    AeT.withObject "M.Vkontakte update object" $ \o -> do
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
                        { replysuccessUpdates = updates
                        , replysuccessTs = ts
                        }
            err = do
                failed <- o .: "failed"
                pure $
                    VkUpdateReplyError
                        {replyerrorFailed = failed, replyerrorTs = ts}
        asum [fmap Right success, fmap Left err]

data VkUpdate =
    VkUpdate
        { updateValue :: AeT.Value
        , updateObject :: VkEvent
        }
  deriving  (Eq, Show)

data VkEvent
    = VEMsg VkMessage
    | VECallback VkMyCallback
    | VEUnexpectedEvent
  deriving  (Eq, Show)

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
            pure $ VkUpdate value event

parseCallback :: AeT.Value -> AeT.Parser VkMyCallback
parseCallback =
    AeT.withObject "Expected message object with payload" $ \msg -> do
        pt <- msg .: "payload" :: AeT.Parser T.Text
        let pbs = E.encodeUtf8 pt
            pVal = decode $ BS.fromStrict pbs :: Maybe AeT.Value
        payload <-
            maybe (fail "unable to parse payload object") pure pVal >>=
            AeT.parseJSON
        text <- msg .: "text"
        from_id <- msg .: "from_id"
        pure $ VkMyCallback from_id text payload
