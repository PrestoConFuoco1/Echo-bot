{-# LANGUAGE
    DeriveGeneric,
    RecordWildCards
    #-}
module Vkontakte.Attachment.Types where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Data.Foldable (asum)
import qualified Data.Text as T (Text, unpack, pack)
import Control.Applicative ((<|>))
import GenericPretty


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
            str -> fmap (VAUnexpectedAtt . VkUnexpectedAtt str) $  o .: ({-TL.toStrict-} str)

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

