{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Telegram.Entity where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isLower)
import Data.Function (on)
import Data.Maybe (isJust)
import qualified Data.Text as T (Text)
import DerivingJSON (BotSelectorModifier (..))
import GHC.Generics
import GenericPretty (PrettyShow (..))
import qualified Stuff as S
import Telegram.ProcessMessage.Types

chatIDfromMsg :: TlMessage -> Integer
chatIDfromMsg = chatID . messageChat

isMediaGroup :: TlMessage -> Bool
isMediaGroup m = isJust $ messageMediaGroupID m

data TlMessage = TlMessage
  { messageMessageID :: Integer,
    messageFrom :: Maybe TlUser,
    messageDate :: Integer,
    messageChat :: TlChat,
    messageText :: Maybe T.Text,
    messageMediaGroupID :: Maybe T.Text,
    messageAnimation :: Maybe TlAnimation,
    messageAudio :: Maybe TlAudio,
    messageDocument :: Maybe TlDocument,
    messagePhoto :: Maybe [TlPhotoSize],
    messageSticker :: Maybe TlSticker,
    messageVideo :: Maybe TlVideo,
    messageVideoNote :: Maybe TlVideoNote,
    messageVoice :: Maybe TlVoice,
    messageCaption :: Maybe T.Text,
    messageContact :: Maybe TlContact,
    messageDice :: Maybe TlDice,
    --game
    messagePoll :: Maybe TlPoll,
    messageVenue :: Maybe TlVenue,
    messageLocation :: Maybe TlLocation
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via (BotSelectorModifier TlMessage)
  deriving anyclass (PrettyShow)

-----------------------------------------------------------
newtype TlChat = TlChat
  { chatID :: Integer
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (BotSelectorModifier TlChat)
  deriving anyclass (PrettyShow)

instance Ord TlChat where
  compare = compare `on` chatID

-----------------------------------------------------------
data TlUser = TlUser
  { userID :: Integer,
    userIsBot :: Bool,
    userFirstName :: T.Text,
    userLastName :: Maybe T.Text,
    userUsername :: Maybe T.Text
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via BotSelectorModifier TlUser
  deriving anyclass (PrettyShow)

instance Eq TlUser where
  (==) = (==) `on` userID

instance Ord TlUser where
  compare = compare `on` userID

data TlCallback = TlCallback
  { callbackID :: T.Text,
    callbackFrom :: TlUser,
    callbackData :: Maybe T.Text,
    callbackMessage :: Maybe TlMessage
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via BotSelectorModifier TlCallback
  deriving anyclass (PrettyShow)
