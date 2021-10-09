{-# LANGUAGE TypeFamilies #-}


module BotClass.BotVkInstance where

import BotClass.Class
import BotClass.ClassTypes
import BotClass.ClassTypesVkInstance
import Vkontakte.Types
import Types

import qualified HTTPRequests as H

import qualified Stuff as S (echo, emptyToNothing, withMaybe)
import Data.Aeson (decode)
import Data.Aeson.Types (Parser, parseJSON, toJSON, parseEither, (.:), withObject)
import Data.Foldable (asum)
import qualified Data.Text.Lazy as TL (Text, pack, intercalate, toStrict)
import qualified Data.Text as T (Text, pack, intercalate)
import qualified App.Logger as L

import Data.Bifunctor (first)

import System.Random (StdGen, newStdGen, randomR)

import qualified Data.ByteString.Lazy as BSL (ByteString)

import qualified App.Handle as D
import Control.Monad.Writer (Writer, tell, runWriter)

instance BotClassUtility Vk where
--    getResult :: s -> Rep s -> Maybe Value
    getResult _ = _VR_updates

--    getMsg :: s -> Upd s -> Maybe (Msg s)
    getMsg _ (VkUpdate _ (VEMsg m)) = Just m
    getMsg _ _ = Nothing

--    getChat :: s -> Msg s -> Maybe (Chat s)
    getChat d m = Nothing
--    getUser :: s -> Msg s -> Maybe (User s)
    getUser d m = Just $ _VM_from_id m
--    getText :: s -> Msg s -> Maybe T.Text
    getText _ = {- S.echo .-} _VM_text
--    getUserID :: s -> User s -> T.Text
    getUserID _ = T.pack . show . _VU_id
--
--    getCallbackQuery :: s -> Upd s -> Maybe (CallbackQuery s)
    getCallbackQuery d (VkUpdate _ (VECallback c)) = Just c
    getCallbackQuery d _ = Nothing
--
--    getCallbackUser :: s -> CallbackQuery s -> User s
    getCallbackUser d = _VMC_from_id
--    getCallbackData :: s -> CallbackQuery s -> Maybe T.Text
    getCallbackData _ = Just . TL.toStrict . _VP_payload . _VMC_payload
--    getCallbackChat :: s -> CallbackQuery s -> Maybe (Chat s)
    getCallbackChat d = const Nothing



instance BotClass Vk where
    takesJSON _ = False

--    parseUpdatesList :: s -> Rep s -> Either String [Upd s]
    parseUpdatesList d rep = do
        val <- maybe (Left "Couldn't parse update list, or failed result") Right $ getResult d rep
        parseEither parseJSON val


    --getUpdatesRequest :: (Monad m) => D.Handle s m -> s -> m H.HTTPRequest
    getUpdatesRequest h s = do
        curTS <- fmap vkTs $ D.getMutState h s
        let constState = D.getConstState h s
            timeout' = timeout $ D.commonEnv h
            fullUrl = vkServer constState
            pars = [("act", Just $ H.PText "a_check"),
                    ("key", Just . H.PLText $ vkKey constState),
                    ("ts", Just $ H.PLText curTS),
                    ("wait", Just . H.PIntg $ fromIntegral timeout')]
        return $ H.Req H.GET fullUrl pars

    --parseHTTPResponse :: s -> BSL.ByteString -> Either String (Rep s)
    parseHTTPResponse _ resp = do -- Either
        val <- maybe (Left "Couldn't parse HTTP response") Right $ decode resp
        repl <- parseEither parseJSON val
        return repl

--    isSuccess :: s -> Rep s -> Bool
    isSuccess _ r = _VR_failed r == Nothing

--    sendTextMsg :: (Monad m) => D.Handle s m -> s -> Maybe (Chat s) -> Maybe (User s) -> T.Text
--        -> m (Either String H.HTTPRequest)
    sendTextMsg h s mChat mUser "" = return $ Left "Unable to send empty message."
    sendTextMsg h s mChat Nothing text = return $ Left "VK: no user supplied, unable to send messages to chats."
    sendTextMsg h s mChat (Just u) text = do
        let method = "messages.send"
            sc = D.getConstState h s
        g <- fmap vkRndGen $ D.getMutState h s
        let (rndInt32, g') = randomR (0, 2^32-1) $ g
            pars = [("user_id", Just . H.PIntg $ _VU_id u), ("message", Just $ H.PText text),
                ("random_id", Just . H.PIntg $ fromIntegral (rndInt32::Integer))] <> defaultVkParams (vkAccessToken sc) (apiVersion sc)
        return $ Right $ fmsg (vkUrl sc) (method, pars)

--    repNumKeyboard :: s -> [Int] -> TL.Text -> H.ParamsList
    repNumKeyboard d lst cmd = [("keyboard", Just $ H.PVal obj)]
      where obj = toJSON $ {-S.echo $-} repNumKeyboardVkTxt' cmd lst

--    epilogue :: (Monad m) => D.Handle s m -> s -> [Upd s] -> Rep s -> m ()
    epilogue h s _ rep = maybe (return ()) m $ _VR_ts rep
         where m = D.modifyMutState h s . f
               f x state = state { vkTs = x }

--    processMessage :: (Monad m) => D.Handle s m -> s -> Msg s -> m (Maybe (m H.HTTPRequest))
    processMessage h s m
     | null atts && maybeText == Nothing =
            D.logError h "Unable to send empty message." >> return Nothing
     | otherwise = do
        let (maybePars, toLog) = runWriter $ attsToParsVk' atts
        mapM_ (D.logEntry h) toLog
        case maybeText of
            Nothing -> S.withMaybe maybePars
                (D.logError h "No text found and unable to send any attachments."
                    >> return Nothing)
                (return . Just . f h)
            Just text -> let justPars = maybe [] id maybePars
                         in  return $ Just (f h justPars)
    
      where
            f :: (Monad m) => D.Handle Vk m -> H.ParamsList -> m H.HTTPRequest
            f h extraPars = do
                sm <- D.getMutState h dummyVk
                let sc = D.getConstState h dummyVk
                    upperRandomIDBound = 2^32-1 :: Int
                    (rndInt32, g') = randomR (0, upperRandomIDBound) $ vkRndGen sm
                    method = "messages.send"
                    user = _VM_from_id m
                    pars = [("user_id", Just . H.PIntg $ _VU_id user), ("message", fmap H.PText maybeText),
                            ("random_id", Just . H.PIntg $ fromIntegral rndInt32)]
                            ++ defaultVkParams (vkAccessToken sc) (apiVersion sc)
 
                D.putMutState h dummyVk $ sm { vkRndGen = g' }
                return $ fmsg (vkUrl sc) (method, extraPars <> pars)

                
            maybeText = S.emptyToNothing $ _VM_text m
            atts = _VM_attachments m



attsToParsVk' :: [VkAttachment] -> Writer [L.LoggerEntry] (Maybe H.ParamsList)
attsToParsVk' atts = do
    let part = partitionVkAttachments atts
        messageSendable =
            map encodeVkAtt (pPhotos part) <>
            map encodeVkAtt (pVideos part) <>
            map encodeVkAtt (pAudios part) <>
            map encodeVkAtt (pDocs part) <>
            map encodeVkAtt (pWalls part) <>
            map encodeVkAtt (pMarkets part)
            -- <> map encodeVkAtt (PPolls part)
        maybePars = either (const Nothing) Just eithPars
        eithPars = asum [
                      sendGift $ pGifts part,
                      sendLink $ pLinks part,
                      sendMarketAlbum $ pMarketAlbums part,
                      sendWallReply $ pWallReplies part,
                      sendSticker $ pStickers part,
                      sendMsgSendable messageSendable
                    ]
        unexpList = pUnexpected part
        maybeUnexpectedAttsMsg = case unexpList of
            [] -> Nothing
            xs -> Just $ "Found " <> (T.pack . show . length $ xs) <> " unexpected attachments."
    maybe (return ()) (tell . (:[]) . (,) L.Warning) maybeUnexpectedAttsMsg
    return $ fmap (:[]) maybePars



encodeVkAtt :: (VkAttMessageSendable a) => a -> T.Text
encodeVkAtt x = getType x <> T.pack (show (getOwnerID x)) <>
    "_" <> T.pack (show $ getID x) <> maybeAccessKey
  where maybeAccessKey = maybe "" ("_" <>) $ getAccessKey x

partitionVkAttachments :: [VkAttachment] -> VkAttsPartition
partitionVkAttachments = foldr f nullPartition
  where f (VAPhoto x) acc       = acc { pPhotos       = x : pPhotos acc }
        f (VAVideo x) acc       = acc { pVideos       = x : pVideos acc }
        f (VAAudio x) acc       = acc { pAudios       = x : pAudios acc }
        f (VADocument x) acc    = acc { pDocs         = x : pDocs acc }
        f (VALink x) acc        = acc { pLinks        = x : pLinks acc }
        f (VAMarket x) acc      = acc { pMarkets      = x : pMarkets acc }
        f (VAMarketAlbum x) acc = acc { pMarketAlbums = x : pMarketAlbums acc }
        f (VAWall x) acc        = acc { pWalls        = x : pWalls acc }
        f (VAWallReply x) acc   = acc { pWallReplies  = x : pWallReplies acc }
        f (VASticker x) acc     = acc { pStickers     = x : pStickers acc }
        f (VAGift x) acc        = acc { pGifts        = x : pGifts acc }
        f (VAUnexpectedAtt x) acc   = acc { pUnexpected   = x : pUnexpected acc }



unableToSend obj [] = Left $ "No " ++ obj ++ "found."
unableToSend obj _  = Left $ "Unable to send " ++ obj ++ "."

sendGift = unableToSend "gift"
sendLink = unableToSend "link"
sendMarketAlbum = unableToSend "market album"
sendWallReply = unableToSend "wall reply"

sendSticker [] = Left "No sticker found."
sendSticker (x:xs) =
    Right $ ("sticker_id", Just . H.PIntg . _VSt_sticker_id $ x)


sendMsgSendable :: [T.Text] -> Either String H.ParamsUnit
sendMsgSendable [] = Left "Empty list of attachments."
sendMsgSendable lst = Right $ ("attachment", Just $ H.PText s)
  where s = T.intercalate "," lst

