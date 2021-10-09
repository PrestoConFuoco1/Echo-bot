{-# LANGUAGE
    DeriveGeneric,
    RecordWildCards
    #-}
module Vkontakte.Keyboard where



import Data.Aeson (encode, decode)
import Data.Aeson.Types
import GHC.Generics (Generic)
import qualified Data.Text.Lazy.Encoding as EL (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL (Text, unpack, pack, toStrict)
import qualified Data.Text as T (Text, unpack, pack)
import GenericPretty
import qualified HTTPRequests as H
import Vkontakte.Entity


data VkKeyboard = VkKeyboard {
    _VKB_inline  :: Bool,
    _VKB_buttons :: [[VkButton]]
    } deriving (Eq, Show, Generic)

instance ToJSON VkKeyboard where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 5 }

{-
instance FromJSON VkKeyboard where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }
-}
----------------------------------------------------

data VkButton = VkButton {
    _VB_color :: TL.Text, -- or maybe make an enum for that?
    _VB_action :: VkButtonActions
    } deriving (Eq, Show, Generic)


instance ToJSON VkButton where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }

{-
instance FromJSON VkButton where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 4 }
-}
----------------------------------------------------
--examples

data VkButtonActions =
      VBACallback VkCallbackButton
    | VBAText VkTextButton
        deriving (Eq, Show)


instance ToJSON VkButtonActions where
    toJSON (VBACallback (VkCallbackButton l p)) =
        object ["type" .= ("callback"::T.Text), "label" .= l, "payload" .= p]
    toJSON (VBAText (VkTextButton l p)) =
        object ["type" .= ("text"::T.Text), "label" .= l, "payload" .= p]

instance FromJSON VkButtonActions where
    parseJSON x = ($ x) $ withObject "button object" $ \o -> do
        t <- o .: "type" :: Parser T.Text
        case t of
            "text" -> fmap VBAText $ parseJSON x
            "callback" -> fmap VBACallback $ parseJSON x


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


instance FromJSON VkTextButton where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }

repNumKeyboardVk :: TL.Text -> [Int] -> TL.Text
repNumKeyboardVk cmd lst = EL.decodeUtf8 $
    encode $ toJSON $ VkKeyboard True $
    [map (VkButton "primary" . VBACallback . repNumButtonVk cmd) lst]

repNumButtonVk :: TL.Text -> Int -> VkCallbackButton
repNumButtonVk cmd n = VkCallbackButton shown $ "{\"" <> cmd <> "\": \"" <> shown <> "\"}"-- (cmd <> " " <> shown)
  where shown = TL.pack $ show n 


repNumKeyboardVkTxt :: TL.Text -> [Int] -> TL.Text
repNumKeyboardVkTxt cmd lst = EL.decodeUtf8 $
    encode $ toJSON $ VkKeyboard True $
    [map (VkButton "primary" . VBAText . repNumButtonVkTxt cmd) lst]

repNumButtonVkTxt :: TL.Text -> Int -> VkTextButton
repNumButtonVkTxt cmd n = VkTextButton shown $ VkPayload (cmd <> " " <> shown)
  where shown = TL.pack $ show n 

repNumKeyboardVkTxt' :: TL.Text -> [Int] -> VkKeyboard
repNumKeyboardVkTxt' cmd lst = VkKeyboard True $ 
    [map (VkButton "primary" . VBAText . repNumButtonVkTxt cmd) lst]

------------------------------------------


------------------------------------------

testVkKeyboard, testVkButton, testVkAction :: T.Text
testVkKeyboard = "{\"inline\":true,\"buttons\":[[" <> testVkButton <> "]]}"
testVkButton = "{\"action\":" <> testVkAction <> ",\"color\":\"primary\"}"
testVkAction = "{\"type\":\"callback\",\"label\":\"Press me\",\"payload\":\"{}\"}"

testVkKeyboard' ="{\"inline\":true,\"buttons\":[[{\"action\":{\"type\":\"callback\",\"label\":\"Press me\",\"payload\":\"{}\"},\"color\":\"primary\"}]]}" 

--------------------------------------------
---------------------------------------------------


