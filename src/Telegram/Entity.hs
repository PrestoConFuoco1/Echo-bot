
{-# LANGUAGE
    DeriveGeneric
    , RecordWildCards
    #-}

module Telegram.Entity where


import Data.Aeson.Types
import GHC.Generics (Generic)
import qualified Data.Text as T (Text)
import GenericPretty
import Telegram.ProcessMessage.Types


chatIDfromMsg :: TlMessage -> Integer
chatIDfromMsg = _TC_id . _TM_chat

isMediaGroup :: TlMessage -> Bool
isMediaGroup m = _TM_media_group_id m /= Nothing




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
    _TM_poll :: Maybe TlPoll,
    _TM_venue :: Maybe TlVenue,
    _TM_location :: Maybe TlLocation
    } deriving (Show, Eq, Generic)

instance FromJSON TlMessage where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 4 }
-----------------------------------------------------------

data TlChat = TlChat {
    _TC_id :: Integer
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

instance FromJSON TlCallback where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }




instance PrettyShow TlUser
instance PrettyShow TlChat

instance PrettyShow TlMessage
instance PrettyShow TlCallback
