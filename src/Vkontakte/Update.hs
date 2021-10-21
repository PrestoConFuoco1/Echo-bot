{-# LANGUAGE DeriveGeneric #-}

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
      { _VR_failed :: Maybe Int
      , _VR_val :: Value
      }
   deriving (Eq, Show, Generic)

instance FromJSON VkReply where
   parseJSON x =
      ($ x) $
      withObject "Vkontakte reply object" $ \o -> do
         failed <- o .:? "failed"
         return $ VkReply failed x

data VkUpdateReplySuccess =
   VkUpdateReplySuccess
      { _VURS_updates :: Value
      , _VURS_ts :: Maybe T.Text
      }
   deriving (Show, Eq, Generic)

data VkUpdateReplyError =
   VkUpdateReplyError
      { _VURE_failed :: Int
      , _VURE_ts :: Maybe T.Text
      }
   deriving (Show, Eq, Generic)

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
             return $
                VkUpdateReplySuccess
                   {_VURS_updates = updates, _VURS_ts = ts}
          err = do
             failed <- o .: "failed"
             return $
                VkUpdateReplyError
                   {_VURE_failed = failed, _VURE_ts = ts}
      asum
         [fmap UpdateResponse success, fmap UpdateError err]

----------------------------------------------
data VkUpdate =
   VkUpdate
      { _VU_value :: Value
      , _VU_object :: VkEvent
      }
   deriving (Eq, Show)

data VkEvent
   = VEMsg VkMessage
   | VECallback VkMyCallback
   | VEUnexpectedEvent
   deriving (Eq, Show)

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
               _ -> return VEUnexpectedEvent
         return $
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
            return
            pVal >>=
         parseJSON
      text <- msg .: "text"
      from_id <- msg .: "from_id"
      return $ VkMyCallback from_id text payload

instance PrettyShow VkReply

instance PrettyShow VkUpdateReplyError

instance PrettyShow VkUpdateReplySuccess
