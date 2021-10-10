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
import Control.Monad.Writer (Writer, runWriter)


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
    let constState = D.getConstState h
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
        sc = D.getConstState h
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
        sc = D.getConstState h
        method = "messages.send"
    randomID <- getRandomID (D.specH h)
    let pars = [unit "user_id" $ _VU_id user,
                mUnit "message" maybeText,
                unit "random_id" randomID]
                ++ defaultVkParams sc
    return $ fmsg (vkUrl sc) (method, attachmentsEtc ++ pars)

       


