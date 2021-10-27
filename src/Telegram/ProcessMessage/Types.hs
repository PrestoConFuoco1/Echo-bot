{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Telegram.ProcessMessage.Types where

import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import qualified Data.Text as T (Text)
import DerivingJSON (BotSelectorModifier (..))
import GHC.Generics (Generic)
import GenericPretty (PrettyShow)

data TlSticker = TlSticker
  { stickerFileID :: T.Text,
    stickerFileUniqueID :: T.Text,
    stickerWidth :: Int,
    stickerHeight :: Int,
    stickerIsAnimated :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via BotSelectorModifier TlSticker
  deriving anyclass (PrettyShow)

newtype TlAudio = TlAudio
  { audioFileID :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via BotSelectorModifier TlAudio
  deriving anyclass (PrettyShow)

newtype TlDocument = TlDocument
  { documentFileID :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via BotSelectorModifier TlDocument
  deriving anyclass (PrettyShow)

newtype TlPhotoSize = TlPhotoSize
  { photosizeFileID :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via BotSelectorModifier TlPhotoSize
  deriving anyclass (PrettyShow)

newtype TlVideo = TlVideo
  { videoFileID :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via BotSelectorModifier TlVideo
  deriving anyclass (PrettyShow)

newtype TlAnimation = TlAnimation
  { animationFileID :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via BotSelectorModifier TlAnimation
  deriving anyclass (PrettyShow)

newtype TlVoice = TlVoice
  { voiceFileID :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via BotSelectorModifier TlVoice
  deriving anyclass (PrettyShow)

newtype TlVideoNote = TlVideoNote
  { videonoteFileID :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via BotSelectorModifier TlVideoNote
  deriving anyclass (PrettyShow)

data TlLocation = TlLocation
  { locationLongitude :: Double,
    locationLatitude :: Double
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via BotSelectorModifier TlLocation
  deriving anyclass (PrettyShow)

data TlVenue = TlVenue
  { venueLocation :: TlLocation,
    venueTitle :: T.Text,
    venueAddress :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via BotSelectorModifier TlVenue
  deriving anyclass (PrettyShow)

data TlContact = TlContact
  { contactPhoneNumber :: T.Text,
    contactFirstName :: T.Text,
    contactLastName :: Maybe T.Text,
    contactUserID :: Maybe Integer,
    contactVcard :: Maybe T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via BotSelectorModifier TlContact
  deriving anyclass (PrettyShow)

data TlPollOption = TlPollOptions
  { polloptionText :: T.Text,
    polloptionVoterCount :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via BotSelectorModifier TlPollOption
  deriving anyclass (PrettyShow)

data TlPoll = TlPoll
  { pollID :: T.Text,
    pollQuestion :: T.Text,
    pollOptions :: [TlPollOption],
    pollTotalVoterCount :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via BotSelectorModifier TlPoll
  deriving anyclass (PrettyShow)

data TlDice = TlDice
  { diceEmoji :: T.Text,
    diceValue :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via BotSelectorModifier TlDice
  deriving anyclass (PrettyShow)
