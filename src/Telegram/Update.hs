{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Telegram.Update (TlReply(..), TlUpdateReplyError(..), TlUpdateReplySuccess(..), TlUpdate (..), TlEvent(..), parseUpdatesResponse1
) where

import Data.Aeson.Types (FromJSON (..), ToJSON (..), Value, parseEither, withObject, (.:))
import Data.Foldable (asum)
import qualified Data.Text as T (Text)
import DerivingJSON (BotSelectorModifier (..))
import GHC.Generics (Generic)
import GenericPretty (PrettyShow)
import Telegram.Types.Entity (TlCallback, TlMessage)

data TlReply = TlReply
  { replyOk :: Bool,
    replyResult :: Maybe Value,
    replyDescription :: Maybe T.Text,
    replyErrorCode :: Maybe Integer
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via BotSelectorModifier TlReply
  deriving anyclass (PrettyShow)

newtype TlUpdateReplySuccess = TlUpdateReplySuccess
  { replysuccessResult :: Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyShow)

data TlUpdateReplyError = TlUpdateReplyError
  { replyerrorErrorCode :: Integer,
    replyerrorDescription :: Maybe T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyShow)

parseUpdatesResponse1 ::
  Value ->
  Either String (Either TlUpdateReplyError TlUpdateReplySuccess)
parseUpdatesResponse1 =
  parseEither $
    withObject "M.Telegram updates object" $ \o -> do
      ok <- o .: "ok"
      if ok
        then do
          res <- o .: "result"
          pure $ Right $
              TlUpdateReplySuccess {replysuccessResult = res}
        else do
          errCode <- o .: "error_code"
          description <- o .: "description"
          pure $ Left $
              TlUpdateReplyError
                { replyerrorErrorCode = errCode,
                  replyerrorDescription = description
                }

data TlUpdate = TlUpdate
  { updateUpdateID :: Integer,
    updateEvent :: TlEvent,
    updateValue :: Value
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
            [ TEMsg <$> (o .: "message"),
              TECallback <$> (o .: "callback_query"),
              pure TEUnexpectedEvent
            ]
        pure $ TlUpdate uid ev value
