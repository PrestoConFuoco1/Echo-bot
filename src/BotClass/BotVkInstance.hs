{-# LANGUAGE
    TypeFamilies
    , RecordWildCards
    #-}


module BotClass.BotVkInstance where

import BotClass.Class
import BotClass.ClassTypesVkInstance
import Vkontakte.Types
import Vkontakte.Exceptions
import Types (timeout)

import HTTPRequests as H

import qualified Stuff as S (emptyToNothing, withMaybe)
import Data.Aeson (decode)
import Data.Aeson.Types as AeT (Parser, parseJSON, toJSON, parseEither)
import qualified Data.Text.Lazy as TL (Text, pack, intercalate, toStrict)
import qualified Data.Text as T (Text, pack, intercalate)
import qualified App.Handle as D
import Control.Monad.Writer (Writer, runWriter)
import Data.List (elem)
import qualified Control.Monad.Catch as C
import GenericPretty
import qualified Exceptions as Ex

instance BotClassUtility Vk where
--    getResult :: s -> Rep s -> Maybe Value
    getResult _ = Just . _VURS_updates

--    getMsg :: s -> Upd s -> Maybe (Msg s)
    getMsg _ VkUpdate {..} = case _VU_object of
        VEMsg m -> Just m
        _       -> Nothing


--   getUpdateValue :: s -> Upd s -> Value
    getUpdateValue _ u = _VU_value u

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
    getCallbackQuery _ VkUpdate {..} = case _VU_object of
        VECallback c -> Just c
        _            -> Nothing
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
    parseUpdatesResponse = parseUpdatesResponse1
    --parseUpdatesList = parseUpdatesList1
--    parseUpdatesValueList :: s -> Rep s -> Either String [Value]
    --handleFailedUpdatesRequest :: (C.MonadThrow m) => D.Handle s m -> Rep s -> m ()
    handleFailedUpdatesRequest = handleFailedUpdatesRequest1
    parseUpdatesValueList s rep = do
        res <- maybe (Left "Couldn't parse update list") Right $ getResult s rep
        AeT.parseEither AeT.parseJSON res



--    parseUpdate :: s -> Value -> Either String (Upd s)
    parseUpdate s = AeT.parseEither AeT.parseJSON



    sendTextMsg = sendTextMsg1
    repNumKeyboard = repNumKeyboard1
    processMessage = processMessage1
    epilogue = epilogue1




--getUpdatesRequest :: (Monad m) => D.Handle s m -> s -> m H.HTTPRequest
getUpdatesRequest1 h s = do
    --curTS <- fmap vkTs $ D.getMutState h s
    curTS <- getTimestamp (D.specH h)
    let constState = D.getConstState h
        timeout' = timeout $ D.commonEnv h
        fullUrl = vkServer constState
        pars = [unit "act" ("a_check" :: T.Text),
                unit "key" $ vkKey constState,
                --unit "key" ("hahahahaha"::T.Text),
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

--    parseUpdatesResponse :: s -> BSL.ByteString -> Either String (UpdateResponse (RepSucc s) (RepErr s))
parseUpdatesResponse1 _ resp = do -- Either
    val <- maybe (Left "Couldn't parse updates response into aeson Value") Right $ decode resp
    repl <- parseUpdatesResponse2 val
    return repl


errorMsg1 = "events history is out of date or losed, ready to use new ts got from vk server"
errorMsg2 = "key is out of date, needed to obtain a new one with getLongPollServer"
errorMsg3 = "information (key, ts) is losed, needed to obtain it with getLongPollServer"

handleFailedUpdatesRequest1 :: (C.MonadThrow m) => D.Handle Vk m -> VkUpdateReplyError -> m ()
handleFailedUpdatesRequest1 h e@(VkUpdateReplyError {..}) =
    let funcName = "handleFailedUpdatesRequest: " in
    case _VURE_failed of
  x | x == 1 -> D.logError h errorMsg1 >> S.withMaybe _VURE_ts
        (D.logError h $ funcName <> "no ts found")
        (\ts -> do
            D.logInfo h $ funcName <> "using new ts"
            putTimestamp (D.specH h) ts)
    | x == 2 -> do
        D.logError h $ funcName <> errorMsg2
        C.throwM KeyOutOfDate_GetNew
    | x == 3 -> do
        D.logError h $ funcName <> errorMsg3
        C.throwM KeyAndTsLosed_GetNew
    | otherwise -> do
        D.logFatal h $ "failed to get updates and unable to handle error"
        D.logFatal h $ defaultPrettyT e
        C.throwM Ex.UnableToHandleError


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
epilogue1 h s _ rep = case _VURS_ts rep of
    Nothing -> return ()
    Just x  -> putTimestamp (D.specH h) x

--    processMessage :: (Monad m) => D.Handle s m -> s -> Msg s -> m (Maybe (m H.HTTPRequest))
processMessage1 h s m
 | null atts && maybeText == Nothing = do
        D.logError h $ funcName <> "Unable to send empty message."
        return Nothing
 | otherwise = do
    let (maybePars, toLog) = runWriter $ attsToParsVk' atts
    D.logDebug h $ funcName <> "processing vk attachments"
    mapM_ (D.logEntry h) toLog
    S.withMaybe maybeText
        (S.withMaybe maybePars
            (D.logError h (funcName <> "no text found and unable to send any attachments.") >> 
             return Nothing)
            (return . Just . processMessageVk h user maybeText))
        (\text -> let justPars = maybe [] id maybePars
            in  return $ Just (processMessageVk h user maybeText justPars))

  where
        funcName = "vk_processMessage: "
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

       


