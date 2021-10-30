{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Telegram.Types.ProcessMessage
    ( TlAnimation(..)
    , TlAudio(..)
    , TlDocument(..)
    , TlPhotoSize(..)
    , TlSticker(..)
    , TlVideo(..)
    , TlVideoNote(..)
    , TlVoice(..)
    , TlContact(..)
    , TlDice(..)
    , TlPoll(..)
    , TlVenue(..)
    , TlLocation(..)
    , TlPollOption(..)
    ) where

import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import qualified Data.Text as T (Text)
import DerivingJSON (BotSelectorModifier(..))
import GHC.Generics (Generic)
import GenericPretty (PrettyShow)

data TlSticker =
    TlSticker
        { stickerFileID :: T.Text
        , stickerFileUniqueID :: T.Text
        , stickerWidth :: Int
        , stickerHeight :: Int
        , stickerIsAnimated :: Bool
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (ToJSON, FromJSON) via BotSelectorModifier TlSticker

newtype TlAudio =
    TlAudio
        { audioFileID :: T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (FromJSON) via BotSelectorModifier TlAudio

newtype TlDocument =
    TlDocument
        { documentFileID :: T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (FromJSON) via BotSelectorModifier TlDocument

newtype TlPhotoSize =
    TlPhotoSize
        { photosizeFileID :: T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (FromJSON) via BotSelectorModifier TlPhotoSize

newtype TlVideo =
    TlVideo
        { videoFileID :: T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (FromJSON) via BotSelectorModifier TlVideo

newtype TlAnimation =
    TlAnimation
        { animationFileID :: T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (FromJSON) via BotSelectorModifier TlAnimation

newtype TlVoice =
    TlVoice
        { voiceFileID :: T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (FromJSON) via BotSelectorModifier TlVoice

newtype TlVideoNote =
    TlVideoNote
        { videonoteFileID :: T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (FromJSON) via BotSelectorModifier TlVideoNote

data TlLocation =
    TlLocation
        { locationLongitude :: Double
        , locationLatitude :: Double
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (FromJSON) via BotSelectorModifier TlLocation

data TlVenue =
    TlVenue
        { venueLocation :: TlLocation
        , venueTitle :: T.Text
        , venueAddress :: T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (FromJSON) via BotSelectorModifier TlVenue

data TlContact =
    TlContact
        { contactPhoneNumber :: T.Text
        , contactFirstName :: T.Text
        , contactLastName :: Maybe T.Text
        , contactUserID :: Maybe Integer
        , contactVcard :: Maybe T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (FromJSON) via BotSelectorModifier TlContact

data TlPollOption =
    TlPollOptions
        { polloptionText :: T.Text
        , polloptionVoterCount :: Int
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (FromJSON) via BotSelectorModifier TlPollOption

data TlPoll =
    TlPoll
        { pollID :: T.Text
        , pollQuestion :: T.Text
        , pollOptions :: [TlPollOption]
        , pollTotalVoterCount :: Int
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (FromJSON) via BotSelectorModifier TlPoll

data TlDice =
    TlDice
        { diceEmoji :: T.Text
        , diceValue :: Int
        }
  deriving  (Show, Eq, Generic)
  deriving (FromJSON) via BotSelectorModifier TlDice
  deriving anyclass (PrettyShow)

