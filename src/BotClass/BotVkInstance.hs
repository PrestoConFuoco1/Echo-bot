{-# LANGUAGE TypeFamilies #-}


module BotClass.BotVkInstance where

import BotClass.Class
import BotClass.ClassTypes
import BotClass.ClassTypesVkInstance
import Vkontakte.Types
import Types

import HTTPRequests as H

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
    getUpdatesRequest = getUpdatesRequest1
    parseHTTPResponse = parseHTTPResponse1
    isSuccess = isSuccess1
    parseUpdatesList = parseUpdatesList1
    sendTextMsg = sendTextMsg1
    repNumKeyboard = repNumKeyboard1
    processMessage = processMessage1
    epilogue = epilogue1



--    parseUpdatesList :: s -> Rep s -> Either String [Upd s]
parseUpdatesList1 d rep = do
    val <- maybe (Left "Couldn't parse update list, or failed result") Right $ getResult d rep
    parseEither parseJSON val


--getUpdatesRequest :: (Monad m) => D.Handle s m -> s -> m H.HTTPRequest
getUpdatesRequest1 h s = do
    --curTS <- fmap vkTs $ D.getMutState h s
    curTS <- getTimestamp (D.specH h)
    let constState = D.getConstState h s
        timeout' = timeout $ D.commonEnv h
        fullUrl = vkServer constState
        pars = [unit "act" ("a_check" :: TL.Text),
                unit "key" $ vkKey constState,
                unit "ts" curTS,
                unit "wait" timeout']
    return $ H.Req H.GET fullUrl pars

--parseHTTPResponse :: s -> BSL.ByteString -> Either String (Rep s)
parseHTTPResponse1 _ resp = do -- Either
    val <- maybe (Left "Couldn't parse HTTP response") Right $ decode resp
    repl <- parseEither parseJSON val
    return repl

--    isSuccess :: s -> Rep s -> Bool
isSuccess1 _ r = _VR_failed r == Nothing

--    sendTextMsg :: (Monad m) => D.Handle s m -> s -> Maybe (Chat s) -> Maybe (User s) -> T.Text
--        -> m (Either String H.HTTPRequest)
sendTextMsg1 h s mChat mUser "" = return $ Left "Unable to send empty message."
sendTextMsg1 h s mChat Nothing text = return $ Left "VK: no user supplied, unable to send messages to chats."
sendTextMsg1 h s mChat (Just u) text = do
    let method = "messages.send"
        sc = D.getConstState h s
    randomID <- getRandomID (D.specH h)
    let pars = [unit "user_id" (_VU_id u),
                unit "message" text,
                unit "random_id" randomID]
                -- ++ defaultVkParams (vkAccessToken sc) (apiVersion sc)
                ++ defaultVkParams sc
    return $ Right $ fmsg (vkUrl sc) (method, pars)

--    repNumKeyboard :: s -> [Int] -> TL.Text -> H.ParamsList
repNumKeyboard1 d lst cmd = [unit "keyboard" obj]
  where obj = toJSON $ repNumKeyboardVkTxt' cmd lst

--    epilogue :: (Monad m) => D.Handle s m -> s -> [Upd s] -> Rep s -> m ()
epilogue1 h s _ rep = case _VR_ts rep of
    Nothing -> return ()
    Just x  -> putTimestamp (D.specH h) x

--    processMessage :: (Monad m) => D.Handle s m -> s -> Msg s -> m (Maybe (m H.HTTPRequest))
processMessage1 h s m
 | null atts && maybeText == Nothing =
        D.logError h "Unable to send empty message." >> return Nothing
 | otherwise = do
    let (maybePars, toLog) = runWriter $ attsToParsVk' atts
    mapM_ (D.logEntry h) toLog
    S.withMaybe maybeText
        (S.withMaybe maybePars
            (D.logError h "No text found and unable to send any attachments." >> 
             return Nothing)
            (return . Just . processMessageVk h user maybeText))
        (\text -> let justPars = maybe [] id maybePars
            in  return $ Just (processMessageVk h user maybeText justPars))

  where
        maybeText = S.emptyToNothing $ _VM_text m
        atts = _VM_attachments m
        user = _VM_from_id m


processMessageVk :: (Monad m) =>
    D.Handle Vk m -> VkUser -> Maybe T.Text -> H.ParamsList -> m H.HTTPRequest
processMessageVk h user maybeText attachmentsEtc = do
    let
        sc = D.getConstState h dummyVk
        method = "messages.send"
    randomID <- getRandomID (D.specH h)
    let pars = [unit "user_id" $ _VU_id user,
                mUnit "message" maybeText,
                unit "random_id" randomID]
                ++ defaultVkParams sc
    return $ fmsg (vkUrl sc) (method, attachmentsEtc ++ pars)

       


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
    Right $ unit "sticker_id" (_VSt_sticker_id x)


sendMsgSendable :: [T.Text] -> Either String H.ParamsUnit
sendMsgSendable [] = Left "Empty list of attachments."
sendMsgSendable lst = Right $ unit "attachment" s
  where s = T.intercalate "," lst

