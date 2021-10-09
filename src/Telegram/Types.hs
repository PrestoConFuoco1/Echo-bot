{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}
module Telegram.Types where

import qualified HTTPRequests as H (ParamsList, ParVal (..))

import Data.Aeson (encode)
import Data.Aeson.Types
import GHC.Generics (Generic)
import Data.Foldable (asum)
import qualified Stuff as S (safeHead)
import qualified Private as S (tlURL)
import qualified Data.Text.Lazy.Encoding as EL (decodeUtf8)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as TL (Text, unpack, pack)
import qualified Data.Text as T (Text, unpack, pack)

import GenericPretty

-----------------------------------------------------------


newtype Tele = Tele ()

dummyTl = Tele ()



data TlConfig = TlConf {
        _TC_updID :: Integer,
        _TC_url :: TL.Text
    } deriving (Show, Generic)

defaultTlConfig = TlConf 332500694 S.tlURL

-----------------------------------------------------------

data TlStateConst = TLSC {
    tlUrl :: TL.Text
    } deriving (Show)

data TlStateMut = TLSM {
    tlUpdateID :: Integer
    } deriving (Show)

getUpdateID' :: TlStateMut -> Integer
getUpdateID' (TLSM x) = x

putUpdateID' :: Integer -> TlStateMut -> TlStateMut
putUpdateID' uid m = m { tlUpdateID = uid }

data TlHandler m = TlHandler {
    getUpdateID :: m Integer,
    putUpdateID :: Integer -> m ()
    }

-----------------------------------------------------------
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

data TlEvent = TEMsg TlMessage | TEEdMsg TlMessage | TECallback TlCallback
    deriving (Show, Eq)
{-
instance ToJSON TlUpdate where
    toJSON TlUpdate{..} = undefined
-}
instance FromJSON TlUpdate where
    parseJSON = withObject "update object" $ \o -> do
        uid <- o .: "update_id"
        ev <- asum [ fmap TEMsg (o .: "message"),
                     fmap TEEdMsg (o .: "edited_message"),
                     fmap TECallback (o .: "callback_query")]
        return $ TlUpdate uid ev
-----------------------------------------------------------

data TlMessage = TlMessage {
    _TM_message_id :: Integer,
    _TM_from :: Maybe TlUser,
    _TM_date :: Integer,
    _TM_chat :: TlChat,
    _TM_text :: Maybe T.Text,
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

-------------------------------------------------

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
-------------------------------------------------------


-------------------------------------------------------

data TlInlineButton = TlInlineButton {
    _TIB_text :: TL.Text,
    _TIB_callback_data :: TL.Text
    } deriving (Show, Eq, Generic)

instance ToJSON TlInlineButton where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 5 }

instance FromJSON TlInlineButton where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }


----------------------------------------------------------

data TlInlineKeyboard = TlInlineKeyboard {
    _TIK_inline_keyboard :: [[TlInlineButton]]
    } deriving (Show, Eq, Generic)


instance ToJSON TlInlineKeyboard where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 5 }

instance FromJSON TlInlineKeyboard where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }

----------------------------------------------------------

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


----------------------------------------------------------

repNumKeyboardTele :: TL.Text -> [Int] -> TL.Text
repNumKeyboardTele cmd lst = EL.decodeUtf8 $
    encode $ toJSON $ TlInlineKeyboard $ [map (repNumButtonTele cmd) lst]


repNumButtonTele :: TL.Text -> Int -> TlInlineButton
repNumButtonTele cmd n = TlInlineButton shown (cmd <> " " <> shown)
  where shown = TL.pack $ show n


repNumKeyboardTele' :: TL.Text -> [Int] -> TlInlineKeyboard
repNumKeyboardTele' cmd lst = TlInlineKeyboard $ [map (repNumButtonTele cmd) lst]
{-
-}
-----------------------------------------------------------

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
{-
data TlPoll = TlPoll {

    } deriving (Show, Eq, Generic)

instance FromJSON TlPoll where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 6 }
-}
data TlDice = TlDice {
    _TD_emoji :: T.Text,
    _TD_value :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON TlDice where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 4 }
--    parseJSON _ = return TlDice
------------------------------------------------------------------
sendMessageTele :: TlMessage -> Either String (T.Text, H.ParamsList)
sendMessageTele m =
    maybe (Left $ "Failed to handle message." ++ show m) Right $
        asum $ map ($ m) 
             [  sendTextTele,
                sendPhotoTele,
                sendStickerTele,
                sendAudioTele,
                sendAnimationTele,
                sendDocumentTele,
                sendVideoTele,
                sendVoiceTele,
                sendVideoNoteTele,
                sendLocationTele,   
                sendVenueTele,
                sendContactTele,
                sendDiceTele
             ]


sendTextTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendTextTele m = do
    text <- _TM_text m
    return ("sendMessage", [("chat_id", justChat m),
                            ("text", Just $ H.PText text)])
sendPhotoTele = sendWithCaption ((>>= S.safeHead) . _TM_photo) _TPS_file_id
                        "sendPhoto" "photo"

sendAudioTele = sendWithCaption _TM_audio _TAu_file_id "sendAudio" "audio"
sendDocumentTele =
    sendWithCaption _TM_document _TDoc_file_id "sendDocument" "document"
sendVideoTele = sendWithCaption _TM_video _TVid_file_id "sendVideo" "video"
sendAnimationTele =
    sendWithCaption _TM_animation _TAni_file_id "sendAnimation" "animation"
sendVoiceTele =
    sendWithCaption _TM_voice _TVoi_file_id "sendVoice" "voice"
sendVideoNoteTele =
    sendWithCaption _TM_video_note _TVNt_file_id "sendVideoNote" "video_note"

sendLocationTele m = do
    location <- _TM_location m
    return ("sendLocation", [("chat_id", justChat m),
                             ("latitude", Just . H.PFloat . _TLoc_latitude $ location),
                             ("longitude", Just . H.PFloat . _TLoc_longitude $ location)])

sendVenueTele m = do
    venue <- _TM_venue m
    return ("sendVenue", [("chat_id", justChat m),
                          ("latitude", Just . H.PFloat . _TLoc_latitude . _TVen_location $ venue),
                          ("longitude", Just . H.PFloat . _TLoc_longitude . _TVen_location $ venue),
                          ("title", Just $ H.PText $ _TVen_title venue),
                          ("address", Just $ H.PText $ _TVen_address venue)])
sendContactTele m = do
    contact <- _TM_contact m
    --let contact = contact' { _TCon_vcard = Just defaultVCard }
    return ("sendContact", [("chat_id", justChat m),
                            ("phone_number", Just $ H.PText $ _TCon_phone_number contact),
                            ("first_name", Just $ H.PText $ _TCon_first_name contact),
                            ("last_name", fmap H.PText $ _TCon_last_name contact),
                            ("vcard", fmap H.PText $ _TCon_vcard contact)])
{-
defaultVCard = "BEGIN:VCARD\nVERSION:4.0\nFN:Julia\nPHOTO;MEDIATYPE=image/jpg:" ++ juliaURL ++ "\nEND:VCARD"
juliaURL = "https://sun9-56.userapi.com/154hNIMjd_Tmdts-Ivpzo13gBTqSg_soIEy76g/k5VJLqOaFvg.jpg"
-}


--sendPollTele m = do
--    poll <- _TM_poll m
--    return ("sendPoll", [])
sendDiceTele m = do
    dice <- _TM_dice m
    return ("sendDice", [("chat_id", justChat m)])

sendStickerTele m = do
    sticker <- _TM_sticker m
    return ("sendSticker", [("chat_id", justChat m),
                            ("sticker", Just $ H.PText $ _TS_file_id sticker)])

sendWithCaption fromMsg getFileID method objName m = do
    let mCaption = _TM_caption m
    obj <- fromMsg m
    return (method, [("chat_id", justChat m),
                     (objName, Just $ H.PText $ getFileID obj),
                     ("caption", fmap H.PText $ mCaption)])


justChat :: TlMessage -> Maybe H.ParVal
justChat m = Just . H.PIntg . _TC_id . _TM_chat $ m




---------------------------------------------------------

data TlUserProfilePhotos = TlUserProfilePhotos {
    _TUPP_total_count :: Integer,
    _TUPP_photos :: [[TlPhotoSize]]

    } deriving (Generic, Show, Eq)



instance FromJSON TlUserProfilePhotos where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 6 }





instance PrettyShow TlConfig
instance PrettyShow TlReply
instance PrettyShow TlUser
instance PrettyShow TlChat
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
instance PrettyShow TlVenue
instance PrettyShow TlLocation

instance PrettyShow TlMessage

