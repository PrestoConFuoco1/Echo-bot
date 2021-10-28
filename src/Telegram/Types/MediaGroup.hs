{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Types.MediaGroup where

import Data.Aeson.Types (ToJSON (..), object, (.=))
import Data.Foldable (asum)
import qualified Data.Text as T
import GHC.Generics
import qualified GenericPretty as GP
import qualified Stuff as S
import Telegram.Types.Entity (TlChat (..), TlMessage (..), TlUser (..))
import Telegram.Types.ProcessMessage (TlPhotoSize (..), audioFileID, documentFileID, videoFileID)

data TlMediaGroupIdentifier = TlMediaGroupIdentifier
  { tmgidChat :: TlChat,
    tmgidUser :: Maybe TlUser,
    tmgidMediaGroupID :: T.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (GP.PrettyShow)

data TlInputMediaPhoto = TlInputMediaPhoto
  { impCaption :: Maybe T.Text,
    impMedia :: T.Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (GP.PrettyShow)

instance ToJSON TlInputMediaPhoto where
  toJSON ph =
    let captionParam =
          maybe [] (\x -> ["caption" .= x]) $
            impCaption ph
     in object $
          [ "type" .= ("photo" :: T.Text),
            "media" .= impMedia ph
          ]
            ++ captionParam

data TlInputMediaVideo = TlInputMediaVideo
  { imvCaption :: Maybe T.Text,
    imvMedia :: T.Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (GP.PrettyShow)

instance ToJSON TlInputMediaVideo where
  toJSON ph =
    let captionParam =
          maybe [] (\x -> ["caption" .= x]) $
            imvCaption ph
     in object $
          [ "type" .= ("video" :: T.Text),
            "media" .= imvMedia ph
          ]
            ++ captionParam

data TlInputMediaDocument = TlInputMediaDocument
  { imdCaption :: Maybe T.Text,
    imdMedia :: T.Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (GP.PrettyShow)

instance ToJSON TlInputMediaDocument where
  toJSON ph =
    let captionParam =
          maybe [] (\x -> ["caption" .= x]) $
            imdCaption ph
     in object $
          [ "type" .= ("document" :: T.Text),
            "media" .= imdMedia ph
          ]
            ++ captionParam

data TlInputMediaAudio = TlInputMediaAudio
  { imaudioCaption :: Maybe T.Text,
    imaudioMedia :: T.Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (GP.PrettyShow)

instance ToJSON TlInputMediaAudio where
  toJSON ph =
    let captionParam =
          maybe [] (\x -> ["caption" .= x]) $
            imaudioCaption ph
     in object $
          [ "type" .= ("audio" :: T.Text),
            "media" .= imaudioMedia ph
          ]
            ++ captionParam

data TlMediaGroupPair = TlMediaGroupPair
  { mgpairIdentifier :: TlMediaGroupIdentifier,
    mgpairItems :: [TlMediaGroupUnit]
  }
  deriving stock (Show, Generic)
  deriving anyclass (GP.PrettyShow)

data TlMediaGroupUnit
  = TlpvPhoto TlInputMediaPhoto
  | TlpvVideo TlInputMediaVideo
  | TlpvDocument TlInputMediaDocument
  | TlpvAudio TlInputMediaAudio
  deriving stock (Show, Generic, Eq)
  deriving anyclass (GP.PrettyShow)

instance ToJSON TlMediaGroupUnit where
  toJSON (TlpvPhoto x) = toJSON x
  toJSON (TlpvVideo x) = toJSON x
  toJSON (TlpvDocument x) = toJSON x
  toJSON (TlpvAudio x) = toJSON x

photoVideoCaption :: TlMediaGroupUnit -> Maybe T.Text
photoVideoCaption (TlpvPhoto (TlInputMediaPhoto {..})) =
  impCaption
photoVideoCaption (TlpvVideo (TlInputMediaVideo {..})) =
  imvCaption
photoVideoCaption (TlpvDocument (TlInputMediaDocument {..})) =
  imdCaption
photoVideoCaption (TlpvAudio (TlInputMediaAudio {..})) =
  imaudioCaption

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
        [ fmap TlpvPhoto mInputMediaPhoto,
          fmap TlpvVideo mInputMediaVideo,
          fmap TlpvDocument mInputMediaDocument,
          fmap TlpvAudio mInputMediaAudio
        ]
