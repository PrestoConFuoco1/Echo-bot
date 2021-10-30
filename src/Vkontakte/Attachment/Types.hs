{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Vkontakte.Attachment.Types
    ( VkAttachment(..)
    , VkAttMessageSendable(..)
    , VkAttsPartition(..)
    , nullPartition
    , VkSticker(..)
    ) where

import Control.Applicative ((<|>))
import Data.Aeson.Types ((.:), (.:?))
import qualified Data.Aeson.Types as AeT
import qualified Data.Text as T (Text)
import DerivingJSON (BotSelectorModifier(..))
import GHC.Generics (Generic)
import GenericPretty
    ( PrettyShow(..)
    , consModifier
    , defaultOptionsL
    , genericPrettyShow
    )

data VkAttachment
    = VAPhoto VkPhoto
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
  deriving  (Show, Eq, Generic)

data VkAttsPartition =
    VkAttsPartition
        { pPhotos :: [VkPhoto]
        , pVideos :: [VkVideo]
        , pAudios :: [VkAudio]
        , pDocs :: [VkDocument]
        , pLinks :: [VkLink]
        , pMarkets :: [VkMarket]
        , pMarketAlbums :: [VkMarketAlbum]
        , pWalls :: [VkWall]
        , pWallReplies :: [VkWallReply]
        , pStickers :: [VkSticker]
        , pGifts :: [VkGift]
        , pUnexpected :: [VkUnexpectedAtt]
        }
  deriving  (Show)

nullPartition :: VkAttsPartition
nullPartition = VkAttsPartition [] [] [] [] [] [] [] [] [] [] [] []

instance AeT.FromJSON VkAttachment where
    parseJSON =
        AeT.withObject "Attachment object" $ \o -> do
            attType <- o .: "type" :: AeT.Parser T.Text
            case attType of
                "photo" -> fmap VAPhoto $ o .: "photo"
                "video" -> fmap VAVideo $ o .: "video"
                "audio" -> fmap VAAudio $ o .: "audio"
                "doc" -> fmap VADocument $ o .: "doc"
                "link" -> fmap VALink $ o .: "link"
                "market" -> fmap VAMarket $ o .: "market"
                "market_album" ->
                    fmap VAMarketAlbum $ o .: "market_album"
                "wall" -> fmap VAWall $ o .: "wall"
                "wall_reply" -> fmap VAWallReply $ o .: "wall_reply"
                "sticker" -> fmap VASticker $ o .: "sticker"
                "gift" -> fmap VAGift $ o .: "gift"
                str ->
                    fmap (VAUnexpectedAtt . VkUnexpectedAtt str) $
                    o .: str

------------------------------------------
data VkPhoto =
    VkPhoto
        { photoOwnerID :: Integer
        , photoID :: Integer
        , photoAccessKey :: Maybe T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (AeT.FromJSON) via BotSelectorModifier VkPhoto

------------------------------------------
data VkVideo =
    VkVideo
        { videoOwnerID :: Integer
        , videoID :: Integer
        , videoAccessKey :: Maybe T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (AeT.FromJSON) via BotSelectorModifier VkVideo

-----------------------------------------------
data VkAudio =
    VkAudio
        { audioOwnerID :: Integer
        , audioID :: Integer
        , audioAccessKey :: Maybe T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (AeT.FromJSON) via BotSelectorModifier VkAudio

----------------------------------------------
data VkDocument =
    VkDocument
        { documentOwnerID :: Integer
        , documentID :: Integer
        , documentAccessKey :: Maybe T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (AeT.FromJSON) via BotSelectorModifier VkDocument

----------------------------------------------
data VkLink =
    VkLink -- how to send this?
        { linkUrl :: T.Text
        , linkTitle :: T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (AeT.FromJSON) via BotSelectorModifier VkLink

----------------------------------------------
data VkMarket =
    VkMarket
        { marketOwnerID :: Integer
        , marketID :: Integer
        , marketAccessKey :: Maybe T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (AeT.FromJSON) via BotSelectorModifier VkMarket

----------------------------------------------
data VkMarketAlbum =
    VkMarketAlbum -- how to send this?
        { _VMAl_owner_id :: Integer
        , _VMAl_id :: Integer
        , _VMAl_access_key :: Maybe T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (AeT.FromJSON) via BotSelectorModifier VkMarketAlbum

----------------------------------------------
data VkWall =
    VkWall
        { wallOwnerID :: Integer
        , wallID :: Integer
        , wallAccessKey :: Maybe T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)

instance AeT.FromJSON VkWall where
    parseJSON =
        AeT.withObject "Vk wall record object" $ \o -> do
            iD <- o .: "id"
            accKey <- o .:? "access_key"
            ownerID <- o .: "owner_id" <|> o .: "to_id"
            pure $ VkWall ownerID iD accKey

----------------------------------------------
data VkWallReply =
    VkWallReply -- how to send this?
        { wallreplyID :: Integer
        , wallreplyAccessKey :: Maybe T.Text
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (AeT.FromJSON) via BotSelectorModifier VkWallReply

-----------------------------------------------
newtype VkSticker =
    VkSticker
        { stickerStickerID :: Integer
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (AeT.FromJSON) via BotSelectorModifier VkSticker

-----------------------------------------------
newtype VkGift =
    VkGift -- how to send this?
        { giftID :: Integer
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)
  deriving (AeT.FromJSON) via BotSelectorModifier VkGift

data VkUnexpectedAtt =
    VkUnexpectedAtt
        { unexpattachmentType :: T.Text
        , unexpattachmentVal :: AeT.Value
        }
  deriving  (Show, Eq, Generic)
  deriving anyclass (PrettyShow)

-------------------------------------------------
class VkAttMessageSendable a where
    getType :: a -> T.Text
    getOwnerID :: a -> Integer
    getID :: a -> Integer
    getAccessKey :: a -> Maybe T.Text

instance VkAttMessageSendable VkPhoto where
    getType _ = "photo"
    getOwnerID = photoOwnerID
    getID = photoID
    getAccessKey = photoAccessKey

instance VkAttMessageSendable VkVideo where
    getType _ = "video"
    getOwnerID = videoOwnerID
    getID = videoID
    getAccessKey = videoAccessKey

instance VkAttMessageSendable VkAudio where
    getType _ = "audio"
    getOwnerID = audioOwnerID
    getID = audioID
    getAccessKey = audioAccessKey

instance VkAttMessageSendable VkDocument where
    getType _ = "doc"
    getOwnerID = documentOwnerID
    getID = documentID
    getAccessKey = documentAccessKey

instance VkAttMessageSendable VkWall where
    getType _ = "wall"
    getOwnerID = wallOwnerID
    getID = wallID
    getAccessKey = wallAccessKey

instance VkAttMessageSendable VkMarket where
    getType _ = "market"
    getOwnerID = marketOwnerID
    getID = marketID
    getAccessKey = marketAccessKey

instance PrettyShow VkAttachment where
    prettyShow =
        genericPrettyShow defaultOptionsL {consModifier = drop 2}
