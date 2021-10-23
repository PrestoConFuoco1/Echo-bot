{-# LANGUAGE DeriveGeneric #-}

module Telegram.Keyboard where

import Data.Aeson (encode)
import Data.Aeson.Types
import qualified Data.Text as T (Text, pack)
import qualified Data.Text.Encoding as E (decodeUtf8)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BS (toStrict)

data TlInlineButton =
   TlInlineButton
      { _TIB_text :: T.Text
      , _TIB_callback_data :: T.Text
      }
   deriving (Show, Eq, Generic)

instance ToJSON TlInlineButton where
   toJSON =
      genericToJSON
         defaultOptions {fieldLabelModifier = drop 5}

instance FromJSON TlInlineButton where
   parseJSON =
      genericParseJSON
         defaultOptions {fieldLabelModifier = drop 5}

----------------------------------------------------------
data TlInlineKeyboard =
   TlInlineKeyboard
      { _TIK_inline_keyboard :: [[TlInlineButton]]
      }
   deriving (Show, Eq, Generic)

instance ToJSON TlInlineKeyboard where
   toJSON =
      genericToJSON
         defaultOptions {fieldLabelModifier = drop 5}

instance FromJSON TlInlineKeyboard where
   parseJSON =
      genericParseJSON
         defaultOptions {fieldLabelModifier = drop 5}

----------------------------------------------------------

repNumButtonTele :: T.Text -> Int -> TlInlineButton
repNumButtonTele cmd n =
   TlInlineButton shown (cmd <> " " <> shown)
  where
    shown = T.pack $ show n

repNumKeyboardTele :: T.Text -> [Int] -> TlInlineKeyboard
repNumKeyboardTele cmd lst =
   TlInlineKeyboard [map (repNumButtonTele cmd) lst]
