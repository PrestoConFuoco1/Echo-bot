{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}

module Telegram.Update where

import Data.Aeson.Types
import Data.Foldable (asum)
import qualified Data.Text as T (Text)
import GHC.Generics (Generic)
import GenericPretty
import Telegram.Entity
import DerivingJSON
import Types

data TlReply =
   TlReply
      { replyOk :: Bool
      , replyResult :: Maybe Value
      , replyDescription :: Maybe T.Text
      , replyErrorCode :: Maybe Integer
      }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via BotSelectorModifier TlReply
    deriving anyclass PrettyShow

newtype TlUpdateReplySuccess =
   TlUpdateReplySuccess
      { replysuccessResult :: Value
      }
    deriving stock (Show, Eq, Generic)
    deriving anyclass PrettyShow

data TlUpdateReplyError =
   TlUpdateReplyError
      { replyerrorErrorCode :: Integer
      , replyerrorDescription :: Maybe T.Text
      }
    deriving stock (Show, Eq, Generic)
    deriving anyclass PrettyShow

parseUpdatesResponse1 ::
      Value
   -> Either String (UpdateResponse TlUpdateReplySuccess TlUpdateReplyError)
parseUpdatesResponse1 =
   parseEither $
   withObject "Telegram updates object" $ \o -> do
      ok <- o .: "ok"
      if ok
         then do
            res <- o .: "result"
            pure $
               UpdateResponse $
               TlUpdateReplySuccess {replysuccessResult = res}
         else do
            errCode <- o .: "error_code"
            description <- o .: "description"
            pure $
               UpdateError $
               TlUpdateReplyError
                  { replyerrorErrorCode = errCode
                  , replyerrorDescription = description
                  }

data TlUpdate =
   TlUpdate
      { updateUpdateID :: Integer
      , updateEvent :: TlEvent
      , updateValue :: Value
      }
    deriving stock (Show, Eq, Generic)

data TlEvent
   = TEMsg TlMessage
   | TECallback TlCallback
   | TEUnexpectedEvent
    deriving stock (Show, Eq, Generic)

instance FromJSON TlUpdate where
   parseJSON value =
      ($ value) $
      withObject "update object" $ \o -> do
         uid <- o .: "update_id"
         ev <-
            asum
               [ TEMsg <$> (o .: "message")
               , TECallback <$> (o .: "callback_query")
               , pure TEUnexpectedEvent
               ]
         pure $ TlUpdate uid ev value

