{-# LANGUAGE
    DeriveGeneric
    #-}
module Vkontakte.Keyboard where


import Data.Aeson.Types
import GHC.Generics (Generic)
import qualified Data.Text.Lazy as TL (Text, pack)
import qualified Data.Text as T (Text)
import Vkontakte.Entity


data VkKeyboard = VkKeyboard {
    _VKB_inline  :: Bool,
    _VKB_buttons :: [[VkButton]]
    } deriving (Eq, Show, Generic)

instance ToJSON VkKeyboard where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 5 }
----------------------------------------------------

data VkButton = VkButton {
    _VB_color :: TL.Text,
    _VB_action :: VkButtonActions
    } deriving (Eq, Show, Generic)


instance ToJSON VkButton where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }

----------------------------------------------------

data VkButtonActions =
      VBACallback VkCallbackButton
    | VBAText VkTextButton
        deriving (Eq, Show)


instance ToJSON VkButtonActions where
    toJSON (VBACallback (VkCallbackButton l p)) =
        object ["type" .= ("callback"::T.Text), "label" .= l, "payload" .= p]
    toJSON (VBAText (VkTextButton l p)) =
        object ["type" .= ("text"::T.Text), "label" .= l, "payload" .= p]

data VkCallbackButton = VkCallbackButton {
    _VCB_label :: TL.Text,
    _VCB_payload :: TL.Text
    } deriving (Show, Eq, Generic)

instance ToJSON VkCallbackButton where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 5 }


instance FromJSON VkCallbackButton where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }


data VkTextButton = VkTextButton {
    _VTB_label :: TL.Text,
    _VTB_payload :: VkPayload
    } deriving (Show, Eq, Generic)

instance ToJSON VkTextButton where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 5 }

repNumButtonVkTxt :: TL.Text -> Int -> VkTextButton
repNumButtonVkTxt cmd n = VkTextButton shown $ VkPayload (cmd <> " " <> shown)
  where shown = TL.pack $ show n 

repNumKeyboardVkTxt' :: TL.Text -> [Int] -> VkKeyboard
repNumKeyboardVkTxt' cmd lst = VkKeyboard True
    [map (VkButton "primary" . VBAText . repNumButtonVkTxt cmd) lst]

------------------------------------------
{-
testVkKeyboard, testVkButton, testVkAction :: T.Text
testVkKeyboard = "{\"inline\":true,\"buttons\":[[" <> testVkButton <> "]]}"
testVkButton = "{\"action\":" <> testVkAction <> ",\"color\":\"primary\"}"
testVkAction = "{\"type\":\"callback\",\"label\":\"Press me\",\"payload\":\"{}\"}"

testVkKeyboard' ="{\"inline\":true,\"buttons\":[[{\"action\":{\"type\":\"callback\",\"label\":\"Press me\",\"payload\":\"{}\"},\"color\":\"primary\"}]]}" 
-}

