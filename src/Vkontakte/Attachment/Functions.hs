module Vkontakte.Attachment.Functions where

import qualified App.Logger as L
import Control.Monad.Writer (Writer, tell)
import Data.Foldable (asum)
import qualified Data.Text as T (Text, intercalate, pack)
import qualified GenericPretty as GP
import HTTPRequests as H
import qualified Stuff as S
import Vkontakte.Attachment.Types

attsToParsVk' ::
      [VkAttachment]
   -> Writer [L.LoggerEntry] (Maybe H.ParamsList)
attsToParsVk' atts = do
   let part = partitionVkAttachments atts
       messageSendable =
          map encodeVkAtt (pPhotos part) <>
          map encodeVkAtt (pVideos part) <>
          map encodeVkAtt (pAudios part) <>
          map encodeVkAtt (pDocs part) <>
          map encodeVkAtt (pWalls part) <>
          map encodeVkAtt (pMarkets part)
       maybePars = either (const Nothing) Just eithPars
       eithPars =
          asum
             [ sendGift $ pGifts part
             , sendLink $ pLinks part
             , sendMarketAlbum $ pMarketAlbums part
             , sendWallReply $ pWallReplies part
             , sendSticker $ pStickers part
             , sendMsgSendable messageSendable
             ]
       unexpList = pUnexpected part
       maybeUnexpectedAttsMsg =
          case unexpList of
             [] -> Nothing
             xs ->
                Just $
                "Found " <>
                (T.pack . show . length $ xs) <>
                " unexpected attachments."
   S.withMaybe maybeUnexpectedAttsMsg (pure ()) $ \u -> do
      tell . (: []) $ (L.Warning, u)
      let f x = (L.Warning, GP.defaultPrettyT x)
      tell $ map f unexpList
   pure $ fmap (: []) maybePars

encodeVkAtt :: (VkAttMessageSendable a) => a -> T.Text
encodeVkAtt x =
   getType x <>
   T.pack (show (getOwnerID x)) <>
   "_" <> T.pack (show $ getID x) <> maybeAccessKey
  where
    maybeAccessKey = maybe "" ("_" <>) $ getAccessKey x

partitionVkAttachments :: [VkAttachment] -> VkAttsPartition
partitionVkAttachments = foldr f nullPartition
  where
    f (VAPhoto x) acc = acc {pPhotos = x : pPhotos acc}
    f (VAVideo x) acc = acc {pVideos = x : pVideos acc}
    f (VAAudio x) acc = acc {pAudios = x : pAudios acc}
    f (VADocument x) acc = acc {pDocs = x : pDocs acc}
    f (VALink x) acc = acc {pLinks = x : pLinks acc}
    f (VAMarket x) acc = acc {pMarkets = x : pMarkets acc}
    f (VAMarketAlbum x) acc =
       acc {pMarketAlbums = x : pMarketAlbums acc}
    f (VAWall x) acc = acc {pWalls = x : pWalls acc}
    f (VAWallReply x) acc =
       acc {pWallReplies = x : pWallReplies acc}
    f (VASticker x) acc =
       acc {pStickers = x : pStickers acc}
    f (VAGift x) acc = acc {pGifts = x : pGifts acc}
    f (VAUnexpectedAtt x) acc =
       acc {pUnexpected = x : pUnexpected acc}

unableToSend :: String -> [a] -> Either String b
unableToSend obj [] = Left $ "No " ++ obj ++ "found."
unableToSend obj _ = Left $ "Unable to send " ++ obj ++ "."

sendGift, sendLink, sendMarketAlbum, sendWallReply ::
      [a] -> Either String b
sendGift = unableToSend "gift"

sendLink = unableToSend "link"

sendMarketAlbum = unableToSend "market album"

sendWallReply = unableToSend "wall reply"

sendSticker :: [VkSticker] -> Either String H.ParamsUnit
sendSticker [] = Left "No sticker found."
sendSticker (x:_) =
   Right $ unit "sticker_id" (_VSt_sticker_id x)

sendMsgSendable :: [T.Text] -> Either String H.ParamsUnit
sendMsgSendable [] = Left "Empty list of attachments."
sendMsgSendable lst = Right $ unit "attachment" s
  where
    s = T.intercalate "," lst
