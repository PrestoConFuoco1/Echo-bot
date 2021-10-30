{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Telegram.Update
    ( TlReply(..)
    , TlUpdateReplyError(..)
    , TlUpdateReplySuccess(..)
    , TlUpdate(..)
    , TlEvent(..)
    , parseUpdatesResponse
    ) where

import Data.Aeson.Types
    ( FromJSON(..)
    , Object
    , Parser
    , ToJSON(..)
    , Value
    , (.:)
    , parseEither
    , withObject
    )
import Data.Foldable (asum)
import qualified Data.Text as T (Text)
import DerivingJSON (BotSelectorModifier(..))
import GHC.Generics (Generic)
import GenericPretty (PrettyShow)
import Telegram.Types.Entity (TlCallback, TlMessage)

data TlReply =
    TlReply
        { replyOk :: Bool
        , replyResult :: Maybe Value
        , replyDescription :: Maybe T.Text
        , replyErrorCode :: Maybe Integer
        }
  deriving (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (ToJSON, FromJSON) via BotSelectorModifier TlReply

newtype TlUpdateReplySuccess =
    TlUpdateReplySuccess
        { replysuccessResult :: Value
        }
  deriving (Show, Eq, Generic)
  deriving anyclass (PrettyShow)

data TlUpdateReplyError =
    TlUpdateReplyError
        { replyerrorErrorCode :: Integer
        , replyerrorDescription :: Maybe T.Text
        }
  deriving (Show, Eq, Generic)
  deriving anyclass (PrettyShow)

parseUpdatesResponse ::
       Value
    -> Either String (Either TlUpdateReplyError TlUpdateReplySuccess)
parseUpdatesResponse =
    parseEither $
    withObject "M.Telegram updates object" $ \o -> do
        ok <- o .: "ok"
        if ok
            then Right <$> parseUpdateReplySuccess o
            else Left <$> parseUpdateReplyError o

parseUpdateReplyError :: Object -> Parser TlUpdateReplyError
parseUpdateReplyError o = do
    errCode <- o .: "error_code"
    description <- o .: "description"
    pure $
        TlUpdateReplyError
            { replyerrorErrorCode = errCode
            , replyerrorDescription = description
            }

parseUpdateReplySuccess :: Object -> Parser TlUpdateReplySuccess
parseUpdateReplySuccess o = do
    res <- o .: "result"
    pure $ TlUpdateReplySuccess {replysuccessResult = res}

data TlUpdate =
    TlUpdate
        { updateUpdateID :: Integer
        , updateEvent :: TlEvent
        , updateValue :: Value
        }
  deriving (Show, Eq, Generic)

data TlEvent
    = TEMsg TlMessage
    | TECallback TlCallback
    | TEUnexpectedEvent
  deriving (Show, Eq, Generic)

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
