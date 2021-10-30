{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Execute.Vkontakte
    (
    ) where

import qualified App.Handle as D
import qualified App.Handle.Vkontakte as HV
import BotTypesClass.VkInstance ()
import qualified Control.Monad.Catch as C (MonadThrow, throwM)
import Control.Monad.Writer (runWriter)
import Data.Aeson.Types as AeT (parseEither, parseJSON, toJSON)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T (Text, pack)
import qualified Environment as Env
import qualified Exceptions as Ex
import Execute.BotClass
import GenericPretty
import HTTP.Types as H
import qualified Messenger as M
import qualified Stuff as S (emptyToNothing, withMaybe)
import Vkontakte

instance BotClassUtility 'M.Vkontakte where
    getResult = Just . replysuccessUpdates
    getMsg VkUpdate {..} =
        case updateObject of
            VEMsg m -> Just m
            _ -> Nothing
    getUpdateValue = updateValue
    getChat _ = Nothing
    getUser m = Just $ messageFromID m
    getText = messageText
    getUserID = T.pack . show . userID
    getCallbackQuery VkUpdate {..} =
        case updateObject of
            VECallback c -> Just c
            _ -> Nothing
    getCallbackUser = mycallbackFromID
    getCallbackData = Just . payloadPayload . mycallbackPayload
    getCallbackChat = const Nothing

instance BotClass 'M.Vkontakte where
    takesJSON = vkTakesJSON
    getUpdatesRequest = getUpdatesRequestVk
    isSuccess = isSuccessVk
    handleFailedUpdatesRequest = handleFailedUpdatesRequestVk
    parseUpdatesValueList rep = do
        res <-
            maybe (Left "Couldn't parse update list") Right $
            getResult @'M.Vkontakte rep
        AeT.parseEither AeT.parseJSON res
    parseUpdate = AeT.parseEither AeT.parseJSON
    sendTextMsg = sendTextMsgVk
    repNumKeyboard = repNumKeyboardVk
    processMessage = processMessageVk
    epilogue = epilogueVk

getUpdatesRequestVk ::
       (Monad m) => D.BotHandler 'M.Vkontakte m -> m H.HTTPRequest
getUpdatesRequestVk h = do
    curTS <- HV.getTimestamp (D.specH h)
    let constState = D.getConstState h
        timeout = Env.getTimeout $ D.commonEnv h
        fullUrl = HV.vkServer constState
        pars =
            [ unit "act" ("a_check" :: T.Text)
            , unit "key" $ HV.vkKey constState
            , unit "ts" curTS
            , unit "wait" timeout
            ]
    pure $ H.Req H.GET fullUrl pars

isSuccessVk :: VkReply -> Bool
isSuccessVk = isNothing . replyFailed

errorMsg1, errorMsg2, errorMsg3 :: T.Text
errorMsg1 =
    "events history is out of date or losed, ready to use new ts got from vk server"

errorMsg2 =
    "key is out of date, needed to obtain a new one with getLongPollServer"

errorMsg3 =
    "information (key, ts) is losed, needed to obtain it with getLongPollServer"

handleFailedUpdatesRequestVk ::
       (C.MonadThrow m)
    => D.BotHandler 'M.Vkontakte m
    -> VkUpdateReplyError
    -> m ()
handleFailedUpdatesRequestVk h e@(VkUpdateReplyError {..}) =
    let funcName = "handleFailedUpdatesRequest: "
        key = HV.vkKey $ D.getConstState h
     in case replyerrorFailed of
            x
                | x == 1 -> do
                    D.logError h errorMsg1
                    S.withMaybe
                        replyerrorTs
                        (D.logError h $ funcName <> "no ts found")
                        (\ts -> do
                             D.logInfo h $ funcName <> "using new ts"
                             HV.putTimestamp (D.specH h) ts)
                | x == 2 -> do
                    D.logError h $ funcName <> errorMsg2
                    D.logError h $
                        funcName <> "key was \"" <> key <> "\""
                    C.throwM KeyOutOfDateGetNew
                | x == 3 -> do
                    D.logError h $ funcName <> errorMsg3
                    C.throwM KeyAndTsLosedGetNew
                | otherwise -> do
                    D.logFatal
                        h
                        "failed to get updates and unable to handle error"
                    D.logFatal h $ defaultPrettyT e
                    C.throwM Ex.UnableToHandleError

sendTextMsgVk ::
       (Monad m)
    => D.BotHandler 'M.Vkontakte m
    -> Maybe VkChat
    -> Maybe VkUser
    -> T.Text
    -> m (Either String H.HTTPRequest)
sendTextMsgVk _ _ _ "" = pure $ Left "Unable to send empty message."
sendTextMsgVk _ _ Nothing _ =
    pure $
    Left "VK: no user supplied, unable to send messages to chats."
sendTextMsgVk h _ (Just u) text = do
    let method = "messages.send"
        sc = D.getConstState h
    randomID <- HV.getRandomID (D.specH h)
    let pars =
            [ unit "user_id" (userID u)
            , unit "message" text
            , unit "random_id" randomID
            ] ++
            HV.defaultVkParams sc
    pure $ Right $ buildHTTP (HV.vkUrl sc) (method, pars)

repNumKeyboardVk :: [Int] -> T.Text -> H.ParamsList
repNumKeyboardVk lst cmd = [unit "keyboard" obj]
  where
    obj = toJSON $ repNumKeyboardVkTxt cmd lst

epilogueVk ::
       (Monad m)
    => D.BotHandler 'M.Vkontakte m
    -> [VkUpdate]
    -> VkUpdateReplySuccess
    -> m ()
epilogueVk h _ rep =
    case replysuccessTs rep of
        Nothing -> pure ()
        Just x -> HV.putTimestamp (D.specH h) x

processMessageVk ::
       (Monad m)
    => D.BotHandler 'M.Vkontakte m
    -> VkMessage
    -> m (Maybe (m H.HTTPRequest))
processMessageVk h m
    | null atts && isNothing maybeText = do
        D.logError h $ funcName <> "Unable to send empty message."
        pure Nothing
    | otherwise = do
        let (maybePars, toLog) = runWriter $ attsToParsVk atts
        D.logDebug h $ funcName <> "processing vk attachments"
        mapM_ (D.logEntry h) toLog
        S.withMaybe
            maybeText
            (S.withMaybe
                 maybePars
                 (do D.logError
                         h
                         (funcName <>
                          "no text found and unable to send any attachments.")
                     pure Nothing)
                 (pure . Just . processMessageVk1 h user maybeText))
            (\_ ->
                 let justPars = fromMaybe [] maybePars
                  in pure $
                     Just
                         (processMessageVk1 h user maybeText justPars))
  where
    funcName = "vk_processMessage: "
    maybeText = S.emptyToNothing $ messageText m
    atts = messageAttachments m
    user = messageFromID m

processMessageVk1 ::
       (Monad m)
    => D.BotHandler 'M.Vkontakte m
    -> VkUser
    -> Maybe T.Text
    -> H.ParamsList
    -> m H.HTTPRequest
processMessageVk1 h user maybeText attachmentsEtc = do
    let sc = D.getConstState h
        method = "messages.send"
    randomID <- HV.getRandomID (D.specH h)
    let pars =
            [ unit "user_id" $ userID user
            , mUnit "message" maybeText
            , unit "random_id" randomID
            ] ++
            HV.defaultVkParams sc
    pure $ buildHTTP (HV.vkUrl sc) (method, attachmentsEtc ++ pars)
