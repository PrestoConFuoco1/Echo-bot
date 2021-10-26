{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module Telegram.MediaGroup.Types where

import Data.Aeson.Types
import Data.Foldable (asum)
import qualified Data.Text as T
import GHC.Generics
import qualified GenericPretty as GP
import qualified Stuff as S
import Telegram.Entity
import Telegram.ProcessMessage.Types

data TlMediaGroupIdentifier =
   TlMediaGroupIdentifier
      { _TMGI_chat :: TlChat
      , _TMGI_user :: Maybe TlUser
      , _TMGI_mediaGroupID :: T.Text
      }
   deriving (Show, Eq, Ord, Generic)

instance GP.PrettyShow TlMediaGroupIdentifier

data TlInputMediaPhoto =
   TlInputMediaPhoto
      { _TIMP_caption :: Maybe T.Text
      , _TIMP_media :: T.Text
      }
   deriving (Show, Generic, Eq)

instance GP.PrettyShow TlInputMediaPhoto

instance ToJSON TlInputMediaPhoto where
   toJSON ph =
      let captionParam =
             maybe [] (\x -> ["caption" .= x]) $
             _TIMP_caption ph
       in object $
          [ "type" .= ("photo" :: T.Text)
          , "media" .= _TIMP_media ph
          ] ++
          captionParam

data TlInputMediaVideo =
   TlInputMediaVideo
      { _TIMV_caption :: Maybe T.Text
      , _TIMV_media :: T.Text
      }
   deriving (Show, Generic, Eq)

instance GP.PrettyShow TlInputMediaVideo

instance ToJSON TlInputMediaVideo where
   toJSON ph =
      let captionParam =
             maybe [] (\x -> ["caption" .= x]) $
             _TIMV_caption ph
       in object $
          [ "type" .= ("video" :: T.Text)
          , "media" .= _TIMV_media ph
          ] ++
          captionParam

data TlInputMediaDocument =
   TlInputMediaDocument
      { _TIMD_caption :: Maybe T.Text
      , _TIMD_media :: T.Text
      }
   deriving (Show, Generic, Eq)

instance GP.PrettyShow TlInputMediaDocument

instance ToJSON TlInputMediaDocument where
   toJSON ph =
      let captionParam =
             maybe [] (\x -> ["caption" .= x]) $
             _TIMD_caption ph
       in object $
          [ "type" .= ("document" :: T.Text)
          , "media" .= _TIMD_media ph
          ] ++
          captionParam

data TlInputMediaAudio =
   TlInputMediaAudio
      { _TIMA_caption :: Maybe T.Text
      , _TIMA_media :: T.Text
      }
   deriving (Show, Generic, Eq)

instance GP.PrettyShow TlInputMediaAudio

instance ToJSON TlInputMediaAudio where
   toJSON ph =
      let captionParam =
             maybe [] (\x -> ["caption" .= x]) $
             _TIMA_caption ph
       in object $
          [ "type" .= ("audio" :: T.Text)
          , "media" .= _TIMA_media ph
          ] ++
          captionParam

data TlMediaGroupPair =
   TlMediaGroupPair
      { _TMGP_identifier :: TlMediaGroupIdentifier
      , _TMGP_items :: [TlMediaGroupUnit]
      }
   deriving (Show, Generic)

instance GP.PrettyShow TlMediaGroupPair

data TlMediaGroupUnit
   = TlpvPhoto TlInputMediaPhoto
   | TlpvVideo TlInputMediaVideo
   | TlpvDocument TlInputMediaDocument
   | TlpvAudio TlInputMediaAudio
   deriving (Show, Generic, Eq)

instance GP.PrettyShow TlMediaGroupUnit

instance ToJSON TlMediaGroupUnit where
   toJSON (TlpvPhoto x) = toJSON x
   toJSON (TlpvVideo x) = toJSON x
   toJSON (TlpvDocument x) = toJSON x
   toJSON (TlpvAudio x) = toJSON x

photoVideoCaption :: TlMediaGroupUnit -> Maybe T.Text
photoVideoCaption (TlpvPhoto (TlInputMediaPhoto {..})) =
   _TIMP_caption
photoVideoCaption (TlpvVideo (TlInputMediaVideo {..})) =
   _TIMV_caption
photoVideoCaption (TlpvDocument (TlInputMediaDocument {..})) =
   _TIMD_caption
photoVideoCaption (TlpvAudio (TlInputMediaAudio {..})) =
   _TIMA_caption

maybeMediaGroupUnit :: TlMessage -> Maybe TlMediaGroupUnit
maybeMediaGroupUnit m =
   let mCaption = messageCaption m
       mPhoto =
          messagePhoto m >>= S.safeHead :: Maybe TlPhotoSize
       mVideo = messageVideo m
       mDocument = messageDocument m
       mAudio = messageAudio m
       mInputMediaPhoto =
          fmap
             (TlInputMediaPhoto mCaption . photosizeFileID)
             mPhoto
       mInputMediaVideo =
          fmap
             (TlInputMediaVideo mCaption . videoFileID)
             mVideo
       mInputMediaDocument =
          fmap
             (TlInputMediaDocument mCaption . documentFileID)
             mDocument
       mInputMediaAudio =
          fmap
             (TlInputMediaAudio mCaption . audioFileID)
             mAudio
    in asum
          [ fmap TlpvPhoto mInputMediaPhoto
          , fmap TlpvVideo mInputMediaVideo
          , fmap TlpvDocument mInputMediaDocument
          , fmap TlpvAudio mInputMediaAudio
          ]
