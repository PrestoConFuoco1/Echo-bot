module Telegram.ProcessMessage.Functions where

import Control.Monad (guard, (>=>))
import Data.Foldable (asum)
import Data.Maybe (isJust)
import qualified Data.Text as T (Text)
import qualified GenericPretty as GP
import HTTPTypes as H
import qualified Stuff as S (safeHead)
import Telegram.Entity (TlMessage (..), chatIDfromMsg)
import Telegram.ProcessMessage.Types

sendMessageTele ::
  TlMessage -> Either String (T.Text, H.ParamsList)
sendMessageTele m =
  maybe
    ( Left $
        "Failed to handle message." ++ GP.defaultPretty m
    )
    Right
    $ asum $
      map
        ($ m)
        [ sendDiceTele,
          sendTextTele,
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
          sendPollTele
        ]

sendWithCaption ::
  (TlMessage -> Maybe a) ->
  (a -> T.Text) ->
  T.Text ->
  T.Text ->
  TlMessage ->
  Maybe (T.Text, ParamsList)
sendWithCaption fromMsg getFileID method objName m = do
  let mCaption = messageCaption m
  obj <- fromMsg m
  Just
    ( method,
      [ unit "chat_id" $ chatIDfromMsg m,
        unit objName $ getFileID obj,
        mUnit "caption" mCaption
      ]
    )

sendTextTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendTextTele m = do
  text <- messageText m
  Just
    ( "sendMessage",
      [unit "chat_id" $ chatIDfromMsg m, unit "text" text]
    )

sendPhotoTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendPhotoTele =
  sendWithCaption
    (messagePhoto >=> S.safeHead)
    photosizeFileID
    "sendPhoto"
    "photo"

sendAudioTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendAudioTele =
  sendWithCaption
    messageAudio
    audioFileID
    "sendAudio"
    "audio"

sendDocumentTele ::
  TlMessage -> Maybe (T.Text, H.ParamsList)
sendDocumentTele =
  sendWithCaption
    messageDocument
    documentFileID
    "sendDocument"
    "document"

sendVideoTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendVideoTele =
  sendWithCaption
    messageVideo
    videoFileID
    "sendVideo"
    "video"

sendAnimationTele ::
  TlMessage -> Maybe (T.Text, H.ParamsList)
sendAnimationTele =
  sendWithCaption
    messageAnimation
    animationFileID
    "sendAnimation"
    "animation"

sendVoiceTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendVoiceTele =
  sendWithCaption
    messageVoice
    voiceFileID
    "sendVoice"
    "voice"

sendVideoNoteTele ::
  TlMessage -> Maybe (T.Text, H.ParamsList)
sendVideoNoteTele =
  sendWithCaption
    messageVideoNote
    videonoteFileID
    "sendVideoNote"
    "video_note"

sendLocationTele ::
  TlMessage -> Maybe (T.Text, H.ParamsList)
sendLocationTele m = do
  location <- messageLocation m
  Just
    ( "sendLocation",
      [ unit "chat_id" $ chatIDfromMsg m,
        unit "latitude" $ locationLatitude location,
        unit "longitude" $ locationLongitude location
      ]
    )

sendVenueTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendVenueTele m = do
  venue <- messageVenue m
  Just
    ( "sendVenue",
      [ unit "chat_id" $ chatIDfromMsg m,
        unit "latitude" $
          locationLatitude . venueLocation $ venue,
        unit "longitude" $
          locationLongitude . venueLocation $ venue,
        unit "title" $ venueTitle venue,
        unit "address" $ venueAddress venue
      ]
    )

sendContactTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendContactTele m = do
  contact <- messageContact m
  Just
    ( "sendContact",
      [ unit "chat_id" $ chatIDfromMsg m,
        unit "phone_number" $ contactPhoneNumber contact,
        unit "first_name" $ contactFirstName contact,
        mUnit "last_name" $ contactLastName contact,
        mUnit "vcard" $ contactVcard contact
      ]
    )

sendPollTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendPollTele m = do
  poll <- messagePoll m
  Just
    ( "sendPoll",
      [ unit "chat_id" $ chatIDfromMsg m,
        unit "question" $ pollQuestion poll,
        unit "options" $
          map polloptionText $ pollOptions poll
      ]
    )

sendDiceTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendDiceTele m = do
  guard (messageText m == Just "/dice" || isJust (messageDice m))
  Just
    ( "sendDice",
      [unit "chat_id" $ chatIDfromMsg m]
    )

sendStickerTele :: TlMessage -> Maybe (T.Text, H.ParamsList)
sendStickerTele m = do
  sticker <- messageSticker m
  Just
    ( "sendSticker",
      [ unit "chat_id" $ chatIDfromMsg m,
        unit "sticker" $ stickerFileID sticker
      ]
    )
