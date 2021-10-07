{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

module VkTypes where


import Data.Aeson (encode, decode)
import Data.Aeson.Types
import GHC.Generics (Generic)
import Data.Foldable (asum)
import System.Random (StdGen)
import qualified Stuff as S (echo)
import qualified Private as S (vkAccessToken, vkGroupID)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as T (Text, unpack, pack, toStrict)
import Control.Applicative ((<|>))

import GenericPretty

----------------------------------------------

data VkConfig = VkConf {
    _VC_vkUrl :: T.Text,
    _VC_accessToken :: T.Text,
    _VC_groupID :: Integer,
    _VC_apiV :: T.Text
    } deriving (Show, Eq, Generic)

defaultVkConfig = VkConf "https://api.vk.com/method/" S.vkAccessToken S.vkGroupID "5.124"

---------------------------------------------

data VkStateConst = VKSC {
    vkKey :: T.Text,
    vkServer :: T.Text,
    vkUrl :: T.Text, -- only for methods
    vkAccessToken :: T.Text,
    vkGroupID :: Integer,
    apiVersion :: T.Text
    } deriving (Show, Eq)

data VkStateMut = VKSM {
    vkTs :: T.Text, -- timestamp
    vkRndGen :: StdGen
    } deriving (Show)


----------------------------------------------

data VkReply = VkReply {
    _VR_ts :: Maybe T.Text,
    _VR_updates :: Maybe Value,
    _VR_failed :: Maybe Int
    } deriving (Eq, Show, Generic)

instance ToJSON VkReply where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }

instance FromJSON VkReply where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 4 }


----------------------------------------------

data VkUpdate = VkUpdate {
    _VU_type :: T.Text,
    _VU_object :: VkEvent
    } deriving (Eq, Show)

data VkEvent = VEMsg VkMessage
             | VEMsgEdit VkMessage
             | VECallback VkMyCallback
             deriving (Eq, Show)

instance ToJSON VkUpdate where
    toJSON = undefined

instance FromJSON VkUpdate where
    parseJSON = withObject "update object" $ \o -> do
        updType <- o .: "type" :: Parser T.Text
        event <- case updType of
-- API version >= 5.103 will be used
            "message_new"  -> do
-- if the field "payload" is present, it is an object and has "callback" field,
-- then it is callback update.
                q <- o .: "object"
                msg <- q .: "message" :: Parser Value
                asum [ fmap VECallback $ parseCallback msg, fmap VEMsg $ parseJSON msg]
                
            "message_edit" -> fmap VEMsgEdit $ o .: "object"
            str -> fail $ T.unpack $ "Failed to parse the event object of type \"" <> str <> "\"."
        return $ VkUpdate updType {-$ S.echo -} event

parseCallback :: Value -> Parser VkMyCallback
parseCallback = withObject "Expected message object with payload" $ \msg -> do
    pt <- msg .: "payload" :: Parser T.Text
    let pbs = {- S.echo $-} encodeUtf8 pt
        pVal = {-S.echo $-} decode pbs :: Maybe Value
    payload <- maybe (fail "unable to parse payload object") return pVal >>= parseJSON
    text <- msg .: "text"
    from_id <- msg .: "from_id"
    return $ VkMyCallback from_id text payload

-- Возможный вариант: любое "message_new" при наличии payload интерпретировать
-- как callback. 
-- Другой - изврат. (кажется)
--------------------------------------------------

data VkMessage = VkMessage {
    _VM_id :: Integer,
    _VM_from_id :: VkUser,
    _VM_text :: Maybe T.Text,
    _VM_attachments :: [VkAttachment]
    --_VM_payload :: Maybe VkMyCallback
    } deriving (Eq, Show, Generic)

{-instance ToJSON VkMessage where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }
-}
instance FromJSON VkMessage where
    parseJSON = fmap fixText . genericParseJSON defaultOptions {
        fieldLabelModifier = drop 4 }
      where fixText m = if _VM_text m == Just ""
                        then m {_VM_text = Nothing}
                        else m

-----------------------------------------------------

data VkChat = VkChat

-----------------------------------------------------

data VkUser = VkUser {
    _VU_id :: Integer
    } deriving (Show, Generic)
-- Eq? Maybe two users are equal if their id is the same?


instance ToJSON VkUser where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }

instance FromJSON VkUser where
    parseJSON x = fmap VkUser $ parseJSON x

instance Eq VkUser where
    (==) u1 u2 = (==) (_VU_id u1) (_VU_id u2)

instance Ord VkUser where
    compare u1 u2 = compare (_VU_id u1) (_VU_id u2)


------------------------------------------------------

data VkKeyboard = VkKeyboard {
    _VKB_inline  :: Bool,
    _VKB_buttons :: [[VkButton]]
    } deriving (Eq, Show, Generic)

instance ToJSON VkKeyboard where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 5 }


instance FromJSON VkKeyboard where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }

----------------------------------------------------

data VkButton = VkButton {
    _VB_color :: T.Text, -- or maybe make an enum for that?
    _VB_action :: VkButtonActions
    } deriving (Eq, Show, Generic)


instance ToJSON VkButton where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }


instance FromJSON VkButton where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 4 }

----------------------------------------------------
--examples

data VkButtonActions = VBACallback VkCallbackButton
                     | VBAText VkTextButton deriving (Eq, Show)


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
    _VCB_label :: T.Text,
    _VCB_payload :: T.Text
    } deriving (Show, Eq, Generic)

instance ToJSON VkCallbackButton where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 5 }


instance FromJSON VkCallbackButton where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }


data VkTextButton = VkTextButton {
    _VTB_label :: T.Text,
    _VTB_payload :: VkPayload
    } deriving (Show, Eq, Generic)

instance ToJSON VkTextButton where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 5 }


instance FromJSON VkTextButton where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }

repNumKeyboardVk :: T.Text -> [Int] -> T.Text
repNumKeyboardVk cmd lst = decodeUtf8 $
    encode $ toJSON $ VkKeyboard True $
    [map (VkButton "primary" . VBACallback . repNumButtonVk cmd) lst]

repNumButtonVk :: T.Text -> Int -> VkCallbackButton
repNumButtonVk cmd n = VkCallbackButton shown $ "{\"" <> cmd <> "\": \"" <> shown <> "\"}"-- (cmd <> " " <> shown)
  where shown = T.pack $ show n 


repNumKeyboardVkTxt :: T.Text -> [Int] -> T.Text
repNumKeyboardVkTxt cmd lst = decodeUtf8 $
    encode $ toJSON $ VkKeyboard True $
    [map (VkButton "primary" . VBAText . repNumButtonVkTxt cmd) lst]

repNumButtonVkTxt :: T.Text -> Int -> VkTextButton
repNumButtonVkTxt cmd n = VkTextButton shown $ VkPayload (cmd <> " " <> shown)
  where shown = T.pack $ show n 

repNumKeyboardVkTxt' :: T.Text -> [Int] -> VkKeyboard
repNumKeyboardVkTxt' cmd lst = VkKeyboard True $ 
    [map (VkButton "primary" . VBAText . repNumButtonVkTxt cmd) lst]

------------------------------------------
data VkMyCallback = VkMyCallback {
    _VMC_from_id :: VkUser,
    _VMC_text    :: Maybe T.Text,
    _VMC_payload :: VkPayload
    } deriving (Show, Eq, Generic)


instance ToJSON VkMyCallback where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 5 }


instance FromJSON VkMyCallback where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }



------------------------------------------

testVkKeyboard, testVkButton, testVkAction :: T.Text
testVkKeyboard = "{\"inline\":true,\"buttons\":[[" <> testVkButton <> "]]}"
testVkButton = "{\"action\":" <> testVkAction <> ",\"color\":\"primary\"}"
testVkAction = "{\"type\":\"callback\",\"label\":\"Press me\",\"payload\":\"{}\"}"

testVkKeyboard' ="{\"inline\":true,\"buttons\":[[{\"action\":{\"type\":\"callback\",\"label\":\"Press me\",\"payload\":\"{}\"},\"color\":\"primary\"}]]}" 

--------------------------------------------

data VkPayload = VkPayload {
    _VP_payload :: T.Text
    } deriving (Show, Eq, Generic)


instance ToJSON VkPayload where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 4 }


instance FromJSON VkPayload where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 4 }

---------------------------------------------------

data VkAttachment =
              VAPhoto VkPhoto
            | VAVideo VkVideo
            | VAAudio VkAudio
            | VADocument VkDocument
            | VALink VkLink
            | VAMarket VkMarket
            | VAMarketAlbum VkMarketAlbum
            | VAWall VkWall
            | VAWallReply VkWallReply
            | VASticker VkSticker
            | VAGift VkGift
            | VAUnexpectedAtt VkUnexpectedAtt
    deriving (Show, Eq, Generic)

data VkAttsPartition = VkAttsPartition {
    pPhotos :: [VkPhoto],
    pVideos :: [VkVideo],
    pAudios :: [VkAudio],
    pDocs   :: [VkDocument],
    pLinks  :: [VkLink],
    pMarkets :: [VkMarket],
    pMarketAlbums :: [VkMarketAlbum],
    pWalls  :: [VkWall],
    pWallReplies :: [VkWallReply],
    pStickers :: [VkSticker],
    pGifts   :: [VkGift],
    pUnexpected :: [VkUnexpectedAtt]
    } deriving (Show)

nullPartition = VkAttsPartition [] [] [] [] [] [] [] [] [] [] [] []


instance FromJSON VkAttachment where
    parseJSON = withObject "Attachment object" $ \o -> do
        attType <- o .: "type" :: Parser T.Text
        case attType of
            "photo" -> fmap VAPhoto $ o .: "photo"
            "video" -> fmap VAVideo $ o .: "video"
            "audio" -> fmap VAAudio $ o .: "audio"
            "doc"   -> fmap VADocument $ o .: "doc"
            "link"  -> fmap VALink $ o .: "link"
            "market" -> fmap VAMarket $ o .: "market"
            "market_album" -> fmap VAMarketAlbum $ o .: "market_album"
            "wall" -> fmap VAWall $ o .: "wall"
            "wall_reply" -> fmap VAWallReply $ o .: "wall_reply"
            "sticker" -> fmap VASticker $ o .: "sticker"
            "gift" -> fmap VAGift $ o .: "gift"
            --str -> fail $ "Unexpected attachment type: " <> str
            str -> fmap (VAUnexpectedAtt . VkUnexpectedAtt str) $  o .: (T.toStrict str)

------------------------------------------

data VkPhoto = VkPhoto {
    _VPh_owner_id :: Integer,
    _VPh_id :: Integer, -- T.Text or Integer?
    -- i think this is all what is needed
    _VPh_access_key :: Maybe T.Text
    } deriving (Show, Eq, Generic)


instance FromJSON VkPhoto where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }

------------------------------------------
data VkVideo = VkVideo {
    _VVd_owner_id :: Integer,
    _VVd_id :: Integer, -- T.Text or Integer?
    -- i think this is all what is needed
    _VVd_access_key :: Maybe T.Text
    } deriving (Show, Eq, Generic)


instance FromJSON VkVideo where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }
---------------------------------------------

data VkAudio = VkAudio {
    _VAu_owner_id :: Integer,
    _VAu_id :: Integer,
    _VAu_access_key :: Maybe T.Text
    } deriving (Show, Eq, Generic)

instance FromJSON VkAudio where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }
----------------------------------------------

data VkDocument = VkDocument {
    _VDoc_owner_id :: Integer,
    _VDoc_id :: Integer,
    _VDoc_access_key :: Maybe T.Text
    } deriving (Show, Eq, Generic)

instance FromJSON VkDocument where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 6 }
----------------------------------------------

data VkLink = VkLink { -- how to send this?
    _VLnk_url :: T.Text,
    _VLnk_title :: T.Text
    } deriving (Show, Eq, Generic)

instance FromJSON VkLink where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 6 }

----------------------------------------------

data VkMarket = VkMarket {
    _VMrk_owner_id :: Integer,
    _VMrk_id :: Integer,
    _VMrk_access_key :: Maybe T.Text
    } deriving (Show, Eq, Generic)

instance FromJSON VkMarket where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 6 }

----------------------------------------------

data VkMarketAlbum = VkMarketAlbum { -- how to send this?
    _VMAl_owner_id :: Integer,
    _VMAl_id :: Integer,
    _VMAl_access_key :: Maybe T.Text
    } deriving (Show, Eq, Generic)


instance FromJSON VkMarketAlbum where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 6 }

----------------------------------------------

data VkWall = VkWall {
    _VWl_owner_id :: Integer,
    --_VWl_to_id :: Integer,
    _VWl_id :: Integer,
    _VWl_access_key :: Maybe T.Text
    } deriving (Show, Eq, Generic)

instance FromJSON VkWall where
    parseJSON = withObject "Vk wall record object" $ \o -> do
        iD <- o .: "id"
        accKey <- o .:? "access_key"
        ownerID <- o .: "owner_id" <|> o .: "to_id"
        return $ VkWall ownerID iD accKey
{-
instance FromJSON VkWall where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }
-}
----------------------------------------------

data VkWallReply = VkWallReply { -- how to send this?
    _VWR_id :: Integer,
    _VWR_access_key :: Maybe T.Text
    } deriving (Show, Eq, Generic)


instance FromJSON VkWallReply where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }

-----------------------------------------------

data VkSticker = VkSticker {
    _VSt_sticker_id :: Integer
    } deriving (Show, Eq, Generic)

instance FromJSON VkSticker where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5 }

-----------------------------------------------

data VkGift = VkGift { -- how to send this?
    _VGft_id :: Integer
    } deriving (Show, Eq, Generic)

instance FromJSON VkGift where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 6 }

data VkUnexpectedAtt = VkUnexpectedAtt {
    _VUEA_type :: T.Text,
    _VUEA_val  :: Value
    } deriving (Show, Eq, Generic)

-------------------------------------------------

class VkAttMessageSendable a where
    getType :: a -> T.Text
    getOwnerID :: a -> Integer
    getID :: a -> Integer
    getAccessKey :: a -> Maybe T.Text


instance VkAttMessageSendable VkPhoto where
    getType x = "photo"
    getOwnerID = _VPh_owner_id
    getID = _VPh_id
    getAccessKey = _VPh_access_key

instance VkAttMessageSendable VkVideo where
    getType x = "video"
    getOwnerID = _VVd_owner_id
    getID = _VVd_id
    getAccessKey = _VVd_access_key

instance VkAttMessageSendable VkAudio where
    getType x = "audio"
    getOwnerID = _VAu_owner_id
    getID = _VAu_id
    getAccessKey = _VAu_access_key

instance VkAttMessageSendable VkDocument where
    getType x = "document"
    getOwnerID = _VDoc_owner_id
    getID = _VDoc_id
    getAccessKey = _VDoc_access_key


instance VkAttMessageSendable VkWall where
    getType x = "wall"
    getOwnerID = _VWl_owner_id
    --getOwnerID = _VWl_to_id
    getID = _VWl_id
    getAccessKey = _VWl_access_key


instance VkAttMessageSendable VkMarket where
    getType x = "market"
    getOwnerID = _VMrk_owner_id
    getID = _VMrk_id
    getAccessKey = _VMrk_access_key

--instance VkAttMessageSendable VkPoll where





instance PrettyShow VkConfig
instance PrettyShow VkReply

instance PrettyShow VkPhoto
instance PrettyShow VkVideo
instance PrettyShow VkAudio
instance PrettyShow VkDocument
instance PrettyShow VkLink
instance PrettyShow VkMarket
instance PrettyShow VkMarketAlbum
instance PrettyShow VkWall
instance PrettyShow VkWallReply
instance PrettyShow VkSticker
instance PrettyShow VkGift
instance PrettyShow VkUnexpectedAtt
instance PrettyShow VkAttachment where
    prettyShow = genericPrettyShow defaultOptionsL {
        consModifier = drop 2
        }
instance PrettyShow VkUser
instance PrettyShow VkMessage

