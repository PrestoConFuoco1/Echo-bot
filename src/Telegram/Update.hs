{-# LANGUAGE DeriveGeneric #-}

module Telegram.Update where

import Data.Aeson.Types
import Data.Foldable (asum)
import qualified Data.Text as T (Text)
import GHC.Generics (Generic)
import GenericPretty
import Telegram.Entity
import Types

data TlReply =
   TlReply
      { _TR_ok :: Bool
      , _TR_result :: Maybe Value
      , _TR_description :: Maybe T.Text
      , _TR_error_code :: Maybe Integer
      }
   deriving (Show, Eq, Generic)

instance ToJSON TlReply where
   toJSON =
      genericToJSON
         defaultOptions {fieldLabelModifier = drop 4}

instance FromJSON TlReply where
   parseJSON =
      genericParseJSON
         defaultOptions {fieldLabelModifier = drop 4}

data TlUpdateReplySuccess =
   TlUpdateReplySuccess
      { _TURS_result :: Value
      }
   deriving (Show, Eq, Generic)

data TlUpdateReplyError =
   TlUpdateReplyError
      { _TURE_error_code :: Integer
      , _TURE_description :: Maybe T.Text
      }
   deriving (Show, Eq, Generic)

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
               TlUpdateReplySuccess {_TURS_result = res}
         else do
            errCode <- o .: "error_code"
            description <- o .: "description"
            pure $
               UpdateError $
               TlUpdateReplyError
                  { _TURE_error_code = errCode
                  , _TURE_description = description
                  }

data TlUpdate =
   TlUpdate
      { _TU_update_id :: Integer
      , _TU_event :: TlEvent
      , _TU_value :: Value
      }
   deriving (Show, Eq)

data TlEvent
   = TEMsg TlMessage
   | TECallback TlCallback
   | TEUnexpectedEvent
   deriving (Show, Eq)

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

instance PrettyShow TlReply

instance PrettyShow TlUpdateReplyError

instance PrettyShow TlUpdateReplySuccess
