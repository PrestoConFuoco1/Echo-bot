{-# LANGUAGE DeriveGeneric #-}

module Vkontakte.Keyboard where

import Data.Aeson.Types
import qualified Data.Text as T (Text, pack)
import GHC.Generics (Generic)
import Vkontakte.Entity

data VkKeyboard =
   VkKeyboard
      { _VKB_inline :: Bool
      , _VKB_buttons :: [[VkButton]]
      }
   deriving (Eq, Show, Generic)

instance ToJSON VkKeyboard where
   toJSON =
      genericToJSON
         defaultOptions {fieldLabelModifier = drop 5}

----------------------------------------------------
data VkButton =
   VkButton
      { _VB_color :: T.Text
      , _VB_action :: VkButtonActions
      }
   deriving (Eq, Show, Generic)

instance ToJSON VkButton where
   toJSON =
      genericToJSON
         defaultOptions {fieldLabelModifier = drop 4}

----------------------------------------------------
data VkButtonActions
   = VBACallback VkCallbackButton
   | VBAText VkTextButton
   deriving (Eq, Show)

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
      { _VCB_label :: T.Text
      , _VCB_payload :: T.Text
      }
   deriving (Show, Eq, Generic)

instance ToJSON VkCallbackButton where
   toJSON =
      genericToJSON
         defaultOptions {fieldLabelModifier = drop 5}

instance FromJSON VkCallbackButton where
   parseJSON =
      genericParseJSON
         defaultOptions {fieldLabelModifier = drop 5}

data VkTextButton =
   VkTextButton
      { _VTB_label :: T.Text
      , _VTB_payload :: VkPayload
      }
   deriving (Show, Eq, Generic)

instance ToJSON VkTextButton where
   toJSON =
      genericToJSON
         defaultOptions {fieldLabelModifier = drop 5}

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

