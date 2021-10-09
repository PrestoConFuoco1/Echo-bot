{-# LANGUAGE
    OverloadedStrings
    , DeriveGeneric
    , RecordWildCards
    #-}
module Telegram.Keyboard where

import Data.Aeson (encode)
import Data.Aeson.Types
import GHC.Generics (Generic)
import qualified Data.Text.Lazy.Encoding as EL (decodeUtf8)
import qualified Data.Text.Lazy as TL (Text, unpack, pack)


data TlInlineButton = TlInlineButton {
    _TIB_text :: TL.Text,
    _TIB_callback_data :: TL.Text
    } deriving (Show, Eq, Generic)

instance ToJSON TlInlineButton where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 5 }

instance FromJSON TlInlineButton where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }


----------------------------------------------------------

data TlInlineKeyboard = TlInlineKeyboard {
    _TIK_inline_keyboard :: [[TlInlineButton]]
    } deriving (Show, Eq, Generic)


instance ToJSON TlInlineKeyboard where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 5 }

instance FromJSON TlInlineKeyboard where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }

----------------------------------------------------------

repNumKeyboardTele :: TL.Text -> [Int] -> TL.Text
repNumKeyboardTele cmd lst = EL.decodeUtf8 $
    encode $ toJSON $ TlInlineKeyboard $ [map (repNumButtonTele cmd) lst]


repNumButtonTele :: TL.Text -> Int -> TlInlineButton
repNumButtonTele cmd n = TlInlineButton shown (cmd <> " " <> shown)
  where shown = TL.pack $ show n


repNumKeyboardTele' :: TL.Text -> [Int] -> TlInlineKeyboard
repNumKeyboardTele' cmd lst = TlInlineKeyboard $ [map (repNumButtonTele cmd) lst]
{-
-}

