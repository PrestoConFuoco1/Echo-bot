{-# LANGUAGE
    TypeFamilies
    , RecordWildCards
    #-}

module BotClass.BotVkInstance where

import BotClass.Class
import BotClass.ClassTypesVkInstance
import Types (timeout)
import Vkontakte
import HTTPRequests as H
import qualified Stuff as S (emptyToNothing, withMaybe)
import Data.Aeson.Types as AeT (parseEither, parseJSON, toJSON)
import qualified Data.Text.Lazy as TL (toStrict, Text)
import qualified Data.Text as T (Text, pack)
import qualified App.Handle as D
import Control.Monad.Writer (runWriter)
import qualified Control.Monad.Catch as C (throwM, MonadThrow)
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
    getChat _ _ = Nothing
--    getUser :: s -> Msg s -> Maybe (User s)
    getUser _ m = Just $ _VM_from_id m
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
    getCallbackUser _ = _VMC_from_id
--    getCallbackData :: s -> CallbackQuery s -> Maybe T.Text
    getCallbackData _ = Just . TL.toStrict . _VP_payload . _VMC_payload
--    getCallbackChat :: s -> CallbackQuery s -> Maybe (Chat s)
    getCallbackChat _ = const Nothing



instance BotClass Vk where
    --takesJSON _ = False
    takesJSON _ = vkTakesJSON
    getUpdatesRequest = getUpdatesRequest1
    isSuccess = isSuccess1
    handleFailedUpdatesRequest = handleFailedUpdatesRequest1
    parseUpdatesValueList s rep = do
        res <- maybe (Left "Couldn't parse update list") Right $ getResult s rep
        AeT.parseEither AeT.parseJSON res
    parseUpdate _ = AeT.parseEither AeT.parseJSON

    sendTextMsg = sendTextMsg1
    repNumKeyboard = repNumKeyboard1
    processMessage = processMessage1
    epilogue = epilogue1




getUpdatesRequest1 :: (Monad m) => D.Handle Vk m -> Vk -> m H.HTTPRequest
getUpdatesRequest1 h _ = do
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

isSuccess1 :: Vk -> VkReply -> Bool
isSuccess1 _ r = _VR_failed r == Nothing

errorMsg1, errorMsg2, errorMsg3 :: T.Text
errorMsg1 = "events history is out of date or losed, ready to use new ts got from vk server"
errorMsg2 = "key is out of date, needed to obtain a new one with getLongPollServer"
errorMsg3 = "information (key, ts) is losed, needed to obtain it with getLongPollServer"

handleFailedUpdatesRequest1 :: (C.MonadThrow m) => D.Handle Vk m -> VkUpdateReplyError -> m ()
handleFailedUpdatesRequest1 h e@(VkUpdateReplyError {..}) =
    let funcName = "handleFailedUpdatesRequest: "
        key = vkKey $ D.getConstState h
    in case _VURE_failed of
  x | x == 1 -> D.logError h errorMsg1 >> S.withMaybe _VURE_ts
        (D.logError h $ funcName <> "no ts found")
        (\ts -> do
            D.logInfo h $ funcName <> "using new ts"
            putTimestamp (D.specH h) ts)
    | x == 2 -> do
        D.logError h $ funcName <> errorMsg2
        D.logError h $ funcName <> "key was \"" <> TL.toStrict key <> "\""
        C.throwM KeyOutOfDate_GetNew
    | x == 3 -> do
        D.logError h $ funcName <> errorMsg3
        C.throwM KeyAndTsLosed_GetNew
    | otherwise -> do
        D.logFatal h $ "failed to get updates and unable to handle error"
        D.logFatal h $ defaultPrettyT e
        C.throwM Ex.UnableToHandleError


sendTextMsg1 ::
    (Monad m) => D.Handle Vk m -> s -> Maybe VkChat -> Maybe VkUser -> T.Text
    -> m (Either String H.HTTPRequest)
sendTextMsg1 _ _ _     _ "" = return $ Left "Unable to send empty message."
sendTextMsg1 _ _ _ Nothing _ = return $ Left "VK: no user supplied, unable to send messages to chats."
sendTextMsg1 h _ _ (Just u) text = do
    let method = "messages.send"
        sc = D.getConstState h
    randomID <- getRandomID (D.specH h)
    let pars = [unit "user_id" (_VU_id u),
                unit "message" text,
                unit "random_id" randomID]
                ++ defaultVkParams sc
    return $ Right $ buildHTTP (vkUrl sc) (method, pars)


repNumKeyboard1 :: Vk -> [Int] -> TL.Text -> H.ParamsList
repNumKeyboard1 _ lst cmd = [unit "keyboard" obj]
  where obj = toJSON $ repNumKeyboardVkTxt' cmd lst

epilogue1 :: (Monad m) => D.Handle Vk m -> Vk -> [VkUpdate] -> VkUpdateReplySuccess -> m ()
epilogue1 h _ _ rep = case _VURS_ts rep of
    Nothing -> return ()
    Just x  -> putTimestamp (D.specH h) x

processMessage1 :: (Monad m) => D.Handle Vk m -> Vk -> VkMessage -> m (Maybe (m H.HTTPRequest))
processMessage1 h _ m
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
        (\_ -> let justPars = maybe [] id maybePars
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
    return $ buildHTTP (vkUrl sc) (method, attachmentsEtc ++ pars)

       


