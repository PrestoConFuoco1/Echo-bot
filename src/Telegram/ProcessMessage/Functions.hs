module Telegram.ProcessMessage.Functions where

import Data.Foldable (asum)
import Data.Maybe (isJust)
import qualified Data.Text as T (Text)
import qualified GenericPretty as GP
import HTTPRequests as H
import qualified Stuff as S (safeHead)
import Telegram.Entity
import Telegram.ProcessMessage.Types
import Control.Monad ((>=>))

sendMessageTele ::
      TlMessage -> Either String (T.Text, H.ParamsList)
sendMessageTele m =
   maybe
      (Left $
       "Failed to handle message." ++ GP.defaultPretty m)
      Right $
   asum $
   map
      ($ m)
      [ sendDiceTele
      , sendTextTele
      , sendPhotoTele
      , sendStickerTele
      , sendAudioTele
      , sendAnimationTele
      , sendDocumentTele
      , sendVideoTele
      , sendVoiceTele
      , sendVideoNoteTele
      , sendLocationTele
      , sendVenueTele
      , sendContactTele
      , sendPollTele
      ]

sendWithCaption ::
      (TlMessage -> Maybe a)
   -> (a -> T.Text)
   -> T.Text
   -> T.Text
   -> TlMessage
   -> Maybe (T.Text, ParamsList)
sendWithCaption fromMsg getFileID method objName m = do
   let mCaption = _TM_caption m
   obj <- fromMsg m
   return
      ( method
      , [ unit "chat_id" $ chatIDfromMsg m
        , unit objName $ getFileID obj
        , mUnit "caption" mCaption
        ])

sendTextTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendTextTele m = do
   text <- _TM_text m
   return
      ( "sendMessage"
      , [unit "chat_id" $ chatIDfromMsg m, unit "text" text])

sendPhotoTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendPhotoTele =
   sendWithCaption
      (_TM_photo >=> S.safeHead)
      _TPS_file_id
      "sendPhoto"
      "photo"

sendAudioTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendAudioTele =
   sendWithCaption
      _TM_audio
      _TAu_file_id
      "sendAudio"
      "audio"

sendDocumentTele ::
      TlMessage -> Maybe (T.Text, H.ParamsList)
sendDocumentTele =
   sendWithCaption
      _TM_document
      _TDoc_file_id
      "sendDocument"
      "document"

sendVideoTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendVideoTele =
   sendWithCaption
      _TM_video
      _TVid_file_id
      "sendVideo"
      "video"

sendAnimationTele ::
      TlMessage -> Maybe (T.Text, H.ParamsList)
sendAnimationTele =
   sendWithCaption
      _TM_animation
      _TAni_file_id
      "sendAnimation"
      "animation"

sendVoiceTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendVoiceTele =
   sendWithCaption
      _TM_voice
      _TVoi_file_id
      "sendVoice"
      "voice"

sendVideoNoteTele ::
      TlMessage -> Maybe (T.Text, H.ParamsList)
sendVideoNoteTele =
   sendWithCaption
      _TM_video_note
      _TVNt_file_id
      "sendVideoNote"
      "video_note"

sendLocationTele ::
      TlMessage -> Maybe (T.Text, H.ParamsList)
sendLocationTele m = do
   location <- _TM_location m
   return
      ( "sendLocation"
      , [ unit "chat_id" $ chatIDfromMsg m
        , unit "latitude" $ _Toc_latitude location
        , unit "longitude" $ _Toc_longitude location
        ])

sendVenueTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendVenueTele m = do
   venue <- _TM_venue m
   return
      ( "sendVenue"
      , [ unit "chat_id" $ chatIDfromMsg m
        , unit "latitude" $
          _Toc_latitude . _TVen_location $ venue
        , unit "longitude" $
          _Toc_longitude . _TVen_location $ venue
        , unit "title" $ _TVen_title venue
        , unit "address" $ _TVen_address venue
        ])

sendContactTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendContactTele m = do
   contact <- _TM_contact m
   return
      ( "sendContact"
      , [ unit "chat_id" $ chatIDfromMsg m
        , unit "phone_number" $ _TCon_phone_number contact
        , unit "first_name" $ _TCon_first_name contact
        , mUnit "last_name" $ _TCon_last_name contact
        , mUnit "vcard" $ _TCon_vcard contact
        ])

sendPollTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendPollTele m = do
   poll <- _TM_poll m
   return
      ( "sendPoll"
      , [ unit "chat_id" $ chatIDfromMsg m
        , unit "question" $ _TPoll_question poll
        , unit "options" $
          map _TPollO_text $ _TPoll_options poll
        ])

sendDiceTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendDiceTele m
 = do
   if _TM_text m == Just "/dice" || isJust (_TM_dice m)
      then return
              ( "sendDice"
              , [unit "chat_id" $ chatIDfromMsg m])
      else Nothing

sendStickerTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendStickerTele m = do
   sticker <- _TM_sticker m
   return
      ( "sendSticker"
      , [ unit "chat_id" $ chatIDfromMsg m
        , unit "sticker" $ _TS_file_id sticker
        ])
