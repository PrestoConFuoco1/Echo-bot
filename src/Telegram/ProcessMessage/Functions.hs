{-# LANGUAGE
    OverloadedStrings
    , DeriveGeneric
    , RecordWildCards
    #-}

module Telegram.ProcessMessage.Functions where

import HTTPRequests as H

import Data.Foldable (asum)
import qualified Stuff as S (safeHead)
import qualified Data.Text as T (Text, unpack, pack)

import Telegram.Entity
import Telegram.ProcessMessage.Types

chatIDfromMsg :: TlMessage -> Integer
chatIDfromMsg = _TC_id . _TM_chat

isMediaGroup :: TlMessage -> Bool
isMediaGroup m = _TM_media_group_id m /= Nothing

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

sendWithCaption fromMsg getFileID method objName m = do
    let mCaption = _TM_caption m
    obj <- fromMsg m
    return (method, [unit "chat_id" $ chatIDfromMsg m,
                     unit objName $ getFileID obj,
                     mUnit "caption" mCaption])


sendTextTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendTextTele m = do
    text <- _TM_text m
    return ("sendMessage", [unit "chat_id" $ chatIDfromMsg m,
                            unit "text" text])

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
    return ("sendLocation", [unit "chat_id" $ chatIDfromMsg m,
                             unit "latitude" $ _TLoc_latitude location,
                             unit "longitude" $ _TLoc_longitude location])

sendVenueTele m = do
    venue <- _TM_venue m
    return ("sendVenue", [unit "chat_id" $ chatIDfromMsg m,
                          unit "latitude" $  _TLoc_latitude . _TVen_location $ venue,
                          unit "longitude" $  _TLoc_longitude . _TVen_location $ venue,
                          unit "title" $ _TVen_title venue,
                          unit "address" $ _TVen_address venue])
sendContactTele m = do
    contact <- _TM_contact m
    return ("sendContact", [unit "chat_id" $ chatIDfromMsg m,
                            unit "phone_number" $ _TCon_phone_number contact,
                            unit "first_name" $ _TCon_first_name contact,
                            mUnit "last_name" $ _TCon_last_name contact,
                            mUnit "vcard" $ _TCon_vcard contact])
{-
defaultVCard = "BEGIN:VCARD\nVERSION:4.0\nFN:Julia\nPHOTO;MEDIATYPE=image/jpg:" ++ juliaURL ++ "\nEND:VCARD"
juliaURL = "https://sun9-56.userapi.com/154hNIMjd_Tmdts-Ivpzo13gBTqSg_soIEy76g/k5VJLqOaFvg.jpg"
-}


--sendPollTele m = do
--    poll <- _TM_poll m
--    return ("sendPoll", [])

sendDiceTele m = do
    dice <- _TM_dice m
    return ("sendDice", [unit "chat_id" $ chatIDfromMsg m])

sendStickerTele m = do
    sticker <- _TM_sticker m
    return ("sendSticker", [unit "chat_id" $ chatIDfromMsg m,
                            unit "sticker" $ _TS_file_id sticker])


