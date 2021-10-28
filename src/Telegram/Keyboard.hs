{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Telegram.Keyboard where

import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import qualified Data.Text as T (Text, pack)
import DerivingJSON (BotSelectorModifier (..))
import GHC.Generics (Generic)

data TlInlineButton = TlInlineButton
  { inlinebuttonText :: T.Text,
    inlinebuttonCallbackData :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via BotSelectorModifier TlInlineButton

newtype TlInlineKeyboard = TlInlineKeyboard
  { inlinekeyboardInlineKeyboard :: [[TlInlineButton]]
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via BotSelectorModifier TlInlineKeyboard

repNumButtonTele :: T.Text -> Int -> TlInlineButton
repNumButtonTele cmd n =
  TlInlineButton shown (cmd <> " " <> shown)
  where
    shown = T.pack $ show n

repNumKeyboardTele :: T.Text -> [Int] -> TlInlineKeyboard
repNumKeyboardTele cmd lst =
  TlInlineKeyboard [map (repNumButtonTele cmd) lst]
