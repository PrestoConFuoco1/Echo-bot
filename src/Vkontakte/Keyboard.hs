{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Vkontakte.Keyboard where

import Data.Aeson.Types
import qualified Data.Text as T (Text, pack)
import GHC.Generics (Generic)
import Vkontakte.Entity
import DerivingJSON

data VkKeyboard =
   VkKeyboard
      { keyboardInline :: Bool
      , keyboardButtons :: [[VkButton]]
      }
    deriving stock (Eq, Show, Generic)
    deriving ToJSON via BotSelectorModifier VkKeyboard

data VkButton =
   VkButton
      { buttonColor :: T.Text
      , buttonAction :: VkButtonActions
      }
    deriving stock (Eq, Show, Generic)
    deriving ToJSON via BotSelectorModifier VkButton

data VkButtonActions
   = VBACallback VkCallbackButton
   | VBAText VkTextButton
    deriving stock (Eq, Show)

instance ToJSON VkButtonActions where
   toJSON (VBACallback (VkCallbackButton l p)) =
      object
         [ "type" .= ("callback" :: T.Text)
         , "label" .= l
         , "payload" .= p
         ]
   toJSON (VBAText (VkTextButton l p)) =
      object
         [ "type" .= ("text" :: T.Text)
         , "label" .= l
         , "payload" .= p
         ]

data VkCallbackButton =
   VkCallbackButton
      { callbackbuttonLabel :: T.Text
      , callbackbuttonPayload :: T.Text
      }
    deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via BotSelectorModifier VkCallbackButton

data VkTextButton =
   VkTextButton
      { textbuttonLabel :: T.Text
      , textbuttonPayload :: VkPayload
      }
    deriving stock (Show, Eq, Generic)
    deriving ToJSON via BotSelectorModifier VkTextButton

repNumButtonVkTxt :: T.Text -> Int -> VkTextButton
repNumButtonVkTxt cmd n =
   VkTextButton shown $ VkPayload (cmd <> " " <> shown)
  where
    shown = T.pack $ show n

repNumKeyboardVkTxt :: T.Text -> [Int] -> VkKeyboard
repNumKeyboardVkTxt cmd lst =
   VkKeyboard
      True
      [ map
           (VkButton "primary" .
            VBAText . repNumButtonVkTxt cmd)
           lst
      ]

