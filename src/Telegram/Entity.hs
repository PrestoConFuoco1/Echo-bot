{-# LANGUAGE
    OverloadedStrings
    , DeriveGeneric
    , RecordWildCards
    #-}

module Telegram.Entity where


import Data.Aeson.Types
import GHC.Generics (Generic)
import Data.Foldable (asum)
import qualified Data.Text as T (Text, unpack, pack)
import GenericPretty
import Telegram.ProcessMessage.Types


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

-----------------------------------------------------------

data TlUpdate = TlUpdate {
    _TU_update_id :: Integer,
    _TU_event :: TlEvent } deriving (Show, Eq)

data TlEvent = TEMsg TlMessage
    -- | TEEdMsg TlMessage
    | TECallback TlCallback
    deriving (Show, Eq)
{-
instance ToJSON TlUpdate where
    toJSON TlUpdate{..} = undefined
-}
instance FromJSON TlUpdate where
    parseJSON = withObject "update object" $ \o -> do
        uid <- o .: "update_id"
        ev <- asum [ fmap TEMsg (o .: "message"),
--                     fmap TEEdMsg (o .: "edited_message"),
                     fmap TECallback (o .: "callback_query")]
        return $ TlUpdate uid ev
-----------------------------------------------------------

data TlMessage = TlMessage {
    _TM_message_id :: Integer,
    _TM_from :: Maybe TlUser,
    _TM_date :: Integer,
    _TM_chat :: TlChat,
    _TM_text :: Maybe T.Text,
    _TM_media_group_id :: Maybe T.Text,
    _TM_animation :: Maybe TlAnimation,
    _TM_audio :: Maybe TlAudio,
    _TM_document :: Maybe TlDocument,
    _TM_photo :: Maybe [TlPhotoSize],
    _TM_sticker :: Maybe TlSticker,
    _TM_video :: Maybe TlVideo,
    _TM_video_note :: Maybe TlVideoNote,
    _TM_voice :: Maybe TlVoice,
    _TM_caption :: Maybe T.Text,
    _TM_contact :: Maybe TlContact,
    _TM_dice :: Maybe TlDice,
    --game
    --poll
    _TM_venue :: Maybe TlVenue,
    _TM_location :: Maybe TlLocation
    } deriving (Show, Eq, Generic)

{-
instance ToJSON TlMessage where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }
-}
instance FromJSON TlMessage where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 4 }
-----------------------------------------------------------

data TlChat = TlChat {
    _TC_id :: Integer
    --_TC_type :: String
    } deriving (Show, Eq, Generic)

instance Ord TlChat where
    compare c1 c2 = compare (_TC_id c1) (_TC_id c2)

instance ToJSON TlChat where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }

instance FromJSON TlChat where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 4 }
-----------------------------------------------------------

data TlUser = TlUser {
    _TUs_id :: Integer,
    _TUs_is_bot :: Bool,
    _TUs_first_name :: T.Text,
    _TUs_last_name :: Maybe T.Text,
    _TUs_username :: Maybe T.Text } deriving (Show, Generic)

instance ToJSON TlUser where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 5 }

instance FromJSON TlUser where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }

instance Eq TlUser where
    (==) u1 u2 = (==) (_TUs_id u1) (_TUs_id u2)

instance Ord TlUser where
    compare u1 u2 = compare (_TUs_id u1) (_TUs_id u2)


data TlCallback = TlCallback {
    _TCB_id :: T.Text,
    _TCB_from :: TlUser,
    _TCB_data :: Maybe T.Text,
    _TCB_message :: Maybe TlMessage
    } deriving (Show, Eq, Generic)
{-
instance ToJSON TlCallback where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 5 }
-}
instance FromJSON TlCallback where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }




instance PrettyShow TlReply
instance PrettyShow TlUser
instance PrettyShow TlChat

instance PrettyShow TlMessage

