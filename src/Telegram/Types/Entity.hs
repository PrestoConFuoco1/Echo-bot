{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Telegram.Types.Entity
    ( TlChat(..)
    , TlMessage(..)
    , TlUser(..)
    , chatIDfromMsg
    , TlCallback(..)
    , isMediaGroup
    ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Function (on)
import Data.Maybe (isJust)
import qualified Data.Text as T (Text)
import DerivingJSON (BotSelectorModifier(..))
import GHC.Generics
import GenericPretty (PrettyShow(..))
import Telegram.Types.MessageContent

chatIDfromMsg :: TlMessage -> Integer
chatIDfromMsg = chatID . messageChat

isMediaGroup :: TlMessage -> Bool
isMediaGroup m = isJust $ messageMediaGroupID m

data TlMessage =
    TlMessage
        { messageMessageID :: Integer
        , messageFrom :: Maybe TlUser
        , messageDate :: Integer
        , messageChat :: TlChat
        , messageText :: Maybe T.Text
        , messageMediaGroupID :: Maybe T.Text
        , messageAnimation :: Maybe TlAnimation
        , messageAudio :: Maybe TlAudio
        , messageDocument :: Maybe TlDocument
        , messagePhoto :: Maybe [TlPhotoSize]
        , messageSticker :: Maybe TlSticker
        , messageVideo :: Maybe TlVideo
        , messageVideoNote :: Maybe TlVideoNote
        , messageVoice :: Maybe TlVoice
        , messageCaption :: Maybe T.Text
        , messageContact :: Maybe TlContact
        , messageDice :: Maybe TlDice
    --game
        , messagePoll :: Maybe TlPoll
        , messageVenue :: Maybe TlVenue
        , messageLocation :: Maybe TlLocation
        }
  deriving (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (FromJSON) via (BotSelectorModifier TlMessage)

-----------------------------------------------------------
newtype TlChat =
    TlChat
        { chatID :: Integer
        }
  deriving (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (FromJSON, ToJSON) via (BotSelectorModifier TlChat)

instance Ord TlChat where
    compare = compare `on` chatID

-----------------------------------------------------------
data TlUser =
    TlUser
        { userID :: Integer
        , userIsBot :: Bool
        , userFirstName :: T.Text
        , userLastName :: Maybe T.Text
        , userUsername :: Maybe T.Text
        }
  deriving (Show, Generic)
  deriving anyclass (PrettyShow)
  deriving (FromJSON, ToJSON) via BotSelectorModifier TlUser

instance Eq TlUser where
    (==) = (==) `on` userID

instance Ord TlUser where
    compare = compare `on` userID

data TlCallback =
    TlCallback
        { callbackID :: T.Text
        , callbackFrom :: TlUser
        , callbackData :: Maybe T.Text
        , callbackMessage :: Maybe TlMessage
        }
  deriving (Show, Eq, Generic)
  deriving (FromJSON) via BotSelectorModifier TlCallback
  deriving anyclass (PrettyShow)

