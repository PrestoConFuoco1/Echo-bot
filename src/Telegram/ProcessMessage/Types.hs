{-# LANGUAGE
    DeriveGeneric
    #-}
module Telegram.ProcessMessage.Types where


import Data.Aeson.Types
import GHC.Generics (Generic)
import qualified Data.Text as T (Text)
import GenericPretty


data TlSticker = TlSticker {
    _TS_file_id :: T.Text,
    _TS_file_unique_id :: T.Text,
    _TS_width :: Int,
    _TS_height :: Int,
    _TS_is_animated :: Bool } deriving (Show, Eq, Generic)

instance ToJSON TlSticker where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }

instance FromJSON TlSticker where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 4 }


data TlAudio = TlAudio {
    _TAu_file_id :: T.Text
    } deriving (Show, Eq, Generic)

instance FromJSON TlAudio where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }

data TlDocument = TlDocument {
    _TDoc_file_id :: T.Text
    } deriving (Show, Eq, Generic)


instance FromJSON TlDocument where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 6 }


data TlPhotoSize = TlPhotoSize {
    _TPS_file_id :: T.Text
    --_TPS_
    } deriving (Show, Eq, Generic)


instance FromJSON TlPhotoSize where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }


data TlVideo = TlVideo {
    _TVid_file_id :: T.Text
    } deriving (Show, Eq, Generic)

instance FromJSON TlVideo where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 6 }

data TlAnimation = TlAnimation {
    _TAni_file_id :: T.Text
    } deriving (Show, Eq, Generic)

instance FromJSON TlAnimation where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 6 }

data TlVoice = TlVoice {
    _TVoi_file_id :: T.Text
    } deriving (Show, Eq, Generic)

instance FromJSON TlVoice where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 6 }

data TlVideoNote = TlVideoNote {
    _TVNt_file_id :: T.Text
    } deriving (Show, Eq, Generic)

instance FromJSON TlVideoNote where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 6 }

data TlLocation = TlLocation {
    _TLoc_longitude :: Double,
    _TLoc_latitude :: Double
    } deriving (Show, Eq, Generic)

instance FromJSON TlLocation where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 6 }

data TlVenue = TlVenue {
    _TVen_location :: TlLocation,
    _TVen_title :: T.Text,
    _TVen_address :: T.Text
    } deriving (Show, Eq, Generic)

instance FromJSON TlVenue where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 6 }

data TlContact = TlContact {
    _TCon_phone_number :: T.Text,
    _TCon_first_name :: T.Text,
    _TCon_last_name :: Maybe T.Text,
    _TCon_user_id :: Maybe Integer,
    _TCon_vcard :: Maybe T.Text
    } deriving (Show, Eq, Generic)

instance FromJSON TlContact where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 6 }

data TlPollOption = TlPollOptions {
    _TPollO_text :: T.Text,
    _TPollO_voter_count :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON TlPollOption where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 8 }

data TlPoll = TlPoll {
    _TPoll_id :: T.Text,
    _TPoll_question :: T.Text,
    _TPoll_options :: [TlPollOption],
    _TPoll_total_voter_count :: Int
    
    } deriving (Show, Eq, Generic)

instance FromJSON TlPoll where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 7 }
{-
-}
data TlDice = TlDice {
    _TD_emoji :: T.Text,
    _TD_value :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON TlDice where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 4 }
--    parseJSON _ = return TlDice


instance PrettyShow TlAnimation
instance PrettyShow TlAudio
instance PrettyShow TlDocument
instance PrettyShow TlPhotoSize
instance PrettyShow TlSticker
instance PrettyShow TlVideo
instance PrettyShow TlVideoNote
instance PrettyShow TlVoice
instance PrettyShow TlContact
instance PrettyShow TlDice
instance PrettyShow TlPollOption
instance PrettyShow TlPoll
instance PrettyShow TlVenue
instance PrettyShow TlLocation


