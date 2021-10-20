
{-# LANGUAGE
    DeriveGeneric
    , RecordWildCards
    #-}

module Telegram.Update where


import Data.Aeson.Types
import GHC.Generics (Generic)
import Data.Foldable (asum)
import qualified Data.Text as T (Text)
import GenericPretty
import Types
import Telegram.Entity

data TlReply = TlReply {
    _TR_ok :: Bool,
    _TR_result :: Maybe Value,
    _TR_description :: Maybe T.Text,
    _TR_error_code :: Maybe Integer
    } deriving (Show, Eq, Generic)

instance ToJSON TlReply where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }

instance FromJSON TlReply where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 4 }

data TlUpdateReplySuccess = TlUpdateReplySuccess {
    _TURS_result :: Value
    } deriving (Show, Eq, Generic)

data TlUpdateReplyError = TlUpdateReplyError {
    _TURE_error_code :: Integer
    , _TURE_description :: Maybe T.Text
    } deriving (Show, Eq, Generic)

parseUpdatesResponse1 :: Value -> Either String (UpdateResponse TlUpdateReplySuccess TlUpdateReplyError)
parseUpdatesResponse1 = parseEither $ withObject "Telegram updates object" $ \o -> do
    ok <- o .: "ok"
    case ok of
        True -> do
            res <- o .: "result"
            return $ UpdateResponse $ TlUpdateReplySuccess {
                _TURS_result = res
                }
        False -> do
            errCode <- o .: "error_code"
            description <- o .: "description"
            return $ UpdateError $ TlUpdateReplyError {
                _TURE_error_code = errCode
                , _TURE_description = description
                }

-----------------------------------------------------------

data TlUpdate = TlUpdate {
    _TU_update_id :: Integer
    , _TU_event :: TlEvent
    , _TU_value :: Value
    } deriving (Show, Eq)

data TlEvent =
    TEMsg TlMessage
    -- | TEEdMsg TlMessage
    | TECallback TlCallback
    | TEUnexpectedEvent
    deriving (Show, Eq)

instance FromJSON TlUpdate where
    parseJSON value = ($ value) $ withObject "update object" $ \o -> do
        uid <- o .: "update_id"
        ev <- asum [ fmap TEMsg (o .: "message")
--                   , fmap TEEdMsg (o .: "edited_message")
                     , fmap TECallback (o .: "callback_query")
                     , return TEUnexpectedEvent
                   ]
        return $ TlUpdate uid ev value

instance PrettyShow TlReply
instance PrettyShow TlUpdateReplyError
instance PrettyShow TlUpdateReplySuccess

