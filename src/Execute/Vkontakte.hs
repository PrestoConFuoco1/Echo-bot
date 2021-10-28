{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Execute.Vkontakte where

import qualified App.Handle as D
import Execute.BotClass
import BotTypesClass.VkInstance ()
import qualified Control.Monad.Catch as C
  ( MonadThrow,
    throwM,
  )
import Control.Monad.Writer (runWriter)
import Data.Aeson.Types as AeT
  ( parseEither,
    parseJSON,
    toJSON,
  )
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T (Text, pack)
import qualified Exceptions as Ex
import GenericPretty
import HTTPTypes as H
import qualified Stuff as S (emptyToNothing, withMaybe)
import qualified Types as Y (Messenger (Vkontakte), timeout)
import Vkontakte

instance BotClassUtility 'Y.Vkontakte where
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
  getCallbackData =
    Just . payloadPayload . mycallbackPayload
  getCallbackChat = const Nothing

instance BotClass 'Y.Vkontakte where
  takesJSON = vkTakesJSON
  getUpdatesRequest = getUpdatesRequest1
  isSuccess = isSuccess1
  handleFailedUpdatesRequest = handleFailedUpdatesRequest1
  parseUpdatesValueList rep = do
    res <-
      maybe (Left "Couldn't parse update list") Right $
        getResult @'Y.Vkontakte rep
    AeT.parseEither AeT.parseJSON res
  parseUpdate = AeT.parseEither AeT.parseJSON
  sendTextMsg = sendTextMsg1
  repNumKeyboard = repNumKeyboard1
  processMessage = processMessage1
  epilogue = epilogue1

getUpdatesRequest1 ::
  (Monad m) => D.BotHandler 'Y.Vkontakte m -> m H.HTTPRequest
getUpdatesRequest1 h =
  do
    curTS <- getTimestamp (D.specH h)
    let constState = D.getConstState h
        timeout' = Y.timeout $ D.commonEnv h
        fullUrl = vkServer constState
        pars =
          [ unit "act" ("a_check" :: T.Text),
            unit "key" $ vkKey constState,
            unit "ts" curTS,
            unit "wait" timeout'
          ]
    pure $ H.Req H.GET fullUrl pars

isSuccess1 :: VkReply -> Bool
isSuccess1 = isNothing . replyFailed

errorMsg1, errorMsg2, errorMsg3 :: T.Text
errorMsg1 =
  "events history is out of date or losed, ready to use new ts got from vk server"
errorMsg2 =
  "key is out of date, needed to obtain a new one with getLongPollServer"
errorMsg3 =
  "information (key, ts) is losed, needed to obtain it with getLongPollServer"

handleFailedUpdatesRequest1 ::
  (C.MonadThrow m) =>
  D.BotHandler 'Y.Vkontakte m ->
  VkUpdateReplyError ->
  m ()
handleFailedUpdatesRequest1 h e@(VkUpdateReplyError {..}) =
  let funcName = "handleFailedUpdatesRequest: "
      key = vkKey $ D.getConstState h
   in case replyerrorFailed of
        x
          | x == 1 -> do
            D.logError h errorMsg1
            S.withMaybe
              replyerrorTs
              (D.logError h $ funcName <> "no ts found")
              ( \ts -> do
                  D.logInfo h $
                    funcName <> "using new ts"
                  putTimestamp (D.specH h) ts
              )
          | x == 2 -> do
            D.logError h $ funcName <> errorMsg2
            D.logError h $
              funcName
                <> "key was \""
                <> key
                <> "\""
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

sendTextMsg1 ::
  (Monad m) =>
  D.BotHandler 'Y.Vkontakte m ->
  Maybe VkChat ->
  Maybe VkUser ->
  T.Text ->
  m (Either String H.HTTPRequest)
sendTextMsg1 _ _ _ "" =
  pure $ Left "Unable to send empty message."
sendTextMsg1 _ _ Nothing _ =
  pure $
    Left
      "VK: no user supplied, unable to send messages to chats."
sendTextMsg1 h _ (Just u) text = do
  let method = "messages.send"
      sc = D.getConstState h
  randomID <- getRandomID (D.specH h)
  let pars =
        [ unit "user_id" (userID u),
          unit "message" text,
          unit "random_id" randomID
        ]
          ++ defaultVkParams sc
  pure $ Right $ buildHTTP (vkUrl sc) (method, pars)

repNumKeyboard1 :: [Int] -> T.Text -> H.ParamsList
repNumKeyboard1 lst cmd = [unit "keyboard" obj]
  where
    obj = toJSON $ repNumKeyboardVkTxt cmd lst

epilogue1 ::
  (Monad m) =>
  D.BotHandler 'Y.Vkontakte m ->
  [VkUpdate] ->
  VkUpdateReplySuccess ->
  m ()
epilogue1 h _ rep =
  case replysuccessTs rep of
    Nothing -> pure ()
    Just x -> putTimestamp (D.specH h) x

processMessage1 ::
  (Monad m) =>
  D.BotHandler 'Y.Vkontakte m ->
  VkMessage ->
  m (Maybe (m H.HTTPRequest))
processMessage1 h m
  | null atts && isNothing maybeText = do
    D.logError h $
      funcName <> "Unable to send empty message."
    pure Nothing
  | otherwise = do
    let (maybePars, toLog) =
          runWriter $ attsToParsVk atts
    D.logDebug h $ funcName <> "processing vk attachments"
    mapM_ (D.logEntry h) toLog
    S.withMaybe
      maybeText
      ( S.withMaybe
          maybePars
          ( do
              D.logError
                h
                ( funcName
                    <> "no text found and unable to send any attachments."
                )
              pure Nothing
          )
          ( pure
              . Just
              . processMessageVk h user maybeText
          )
      )
      ( \_ ->
          let justPars = fromMaybe [] maybePars
           in pure $
                Just
                  ( processMessageVk
                      h
                      user
                      maybeText
                      justPars
                  )
      )
  where
    funcName = "vk_processMessage: "
    maybeText = S.emptyToNothing $ messageText m
    atts = messageAttachments m
    user = messageFromID m

processMessageVk ::
  (Monad m) =>
  D.BotHandler 'Y.Vkontakte m ->
  VkUser ->
  Maybe T.Text ->
  H.ParamsList ->
  m H.HTTPRequest
processMessageVk h user maybeText attachmentsEtc = do
  let sc = D.getConstState h
      method = "messages.send"
  randomID <- getRandomID (D.specH h)
  let pars =
        [ unit "user_id" $ userID user,
          mUnit "message" maybeText,
          unit "random_id" randomID
        ]
          ++ defaultVkParams sc
  pure $
    buildHTTP (vkUrl sc) (method, attachmentsEtc ++ pars)