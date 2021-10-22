{-# LANGUAGE TypeFamilies, RecordWildCards #-}

module BotClass.BotVkInstance where

import Data.Maybe (isNothing, fromMaybe)
import qualified App.Handle as D
import BotClass.Class
import BotClass.ClassTypesVkInstance
import qualified Control.Monad.Catch as C
   ( MonadThrow
   , throwM
   )
import Control.Monad.Writer (runWriter)
import Data.Aeson.Types as AeT
   ( parseEither
   , parseJSON
   , toJSON
   )
import qualified Data.Text as T (Text, pack)
import qualified Exceptions as Ex
import GenericPretty
import HTTPRequests as H
import qualified Stuff as S (emptyToNothing, withMaybe)
import Types (timeout)
import Vkontakte

instance BotClassUtility Vk where
   getResult _ = Just . _VURS_updates
   getMsg _ VkUpdate {..} =
      case _VU_object of
         VEMsg m -> Just m
         _ -> Nothing
   getUpdateValue _ = _VU_value
   getChat _ _ = Nothing
   getUser _ m = Just $ _VM_from_id m
   getText _ = _VM_text
   getUserID _ = T.pack . show . _VU_id
   getCallbackQuery _ VkUpdate {..} =
      case _VU_object of
         VECallback c -> Just c
         _ -> Nothing
   getCallbackUser _ = _VMC_from_id
   getCallbackData _ =
      Just . _VP_payload . _VMC_payload
   getCallbackChat _ = const Nothing

instance BotClass Vk
                          where
   takesJSON _ = vkTakesJSON
   getUpdatesRequest = getUpdatesRequest1
   isSuccess = isSuccess1
   handleFailedUpdatesRequest = handleFailedUpdatesRequest1
   parseUpdatesValueList s rep = do
      res <-
         maybe (Left "Couldn't parse update list") Right $
         getResult s rep
      AeT.parseEither AeT.parseJSON res
   parseUpdate _ = AeT.parseEither AeT.parseJSON
   sendTextMsg = sendTextMsg1
   repNumKeyboard = repNumKeyboard1
   processMessage = processMessage1
   epilogue = epilogue1

getUpdatesRequest1 ::
      (Monad m) => D.Handle Vk m -> Vk -> m H.HTTPRequest
getUpdatesRequest1 h _
 = do
   curTS <- getTimestamp (D.specH h)
   let constState = D.getConstState h
       timeout' = timeout $ D.commonEnv h
       fullUrl = vkServer constState
       pars =
          [ unit "act" ("a_check" :: T.Text)
          , unit "key" $ vkKey constState
          , unit "ts" curTS
          , unit "wait" timeout'
          ]
   return $ H.Req H.GET fullUrl pars

isSuccess1 :: Vk -> VkReply -> Bool
--isSuccess1 _ r = _VR_failed r == Nothing
isSuccess1 _ = isNothing . _VR_failed

errorMsg1, errorMsg2, errorMsg3 :: T.Text
errorMsg1 =
   "events history is out of date or losed, ready to use new ts got from vk server"

errorMsg2 =
   "key is out of date, needed to obtain a new one with getLongPollServer"

errorMsg3 =
   "information (key, ts) is losed, needed to obtain it with getLongPollServer"

handleFailedUpdatesRequest1 ::
      (C.MonadThrow m)
   => D.Handle Vk m
   -> VkUpdateReplyError
   -> m ()
handleFailedUpdatesRequest1 h e@(VkUpdateReplyError {..}) =
   let funcName = "handleFailedUpdatesRequest: "
       key = vkKey $ D.getConstState h
    in case _VURE_failed of
          x
             | x == 1 -> do
                D.logError h errorMsg1
                S.withMaybe
                   _VURE_ts
                   (D.logError h $ funcName <> "no ts found")
                   (\ts -> do
                       D.logInfo h $
                          funcName <> "using new ts"
                       putTimestamp (D.specH h) ts)
             | x == 2 -> do
                D.logError h $ funcName <> errorMsg2
                D.logError h $
                   funcName <>
                   "key was \"" <> key <> "\""
                C.throwM KeyOutOfDate_GetNew
             | x == 3 -> do
                D.logError h $ funcName <> errorMsg3
                C.throwM KeyAndTsLosed_GetNew
             | otherwise -> do
                D.logFatal
                   h
                   "failed to get updates and unable to handle error"
                D.logFatal h $ defaultPrettyT e
                C.throwM Ex.UnableToHandleError

sendTextMsg1 ::
      (Monad m)
   => D.Handle Vk m
   -> s
   -> Maybe VkChat
   -> Maybe VkUser
   -> T.Text
   -> m (Either String H.HTTPRequest)
sendTextMsg1 _ _ _ _ "" =
   return $ Left "Unable to send empty message."
sendTextMsg1 _ _ _ Nothing _ =
   return $
   Left
      "VK: no user supplied, unable to send messages to chats."
sendTextMsg1 h _ _ (Just u) text = do
   let method = "messages.send"
       sc = D.getConstState h
   randomID <- getRandomID (D.specH h)
   let pars =
          [ unit "user_id" (_VU_id u)
          , unit "message" text
          , unit "random_id" randomID
          ] ++
          defaultVkParams sc
   return $ Right $ buildHTTP (vkUrl sc) (method, pars)

repNumKeyboard1 :: Vk -> [Int] -> T.Text -> H.ParamsList
repNumKeyboard1 _ lst cmd = [unit "keyboard" obj]
  where
    obj = toJSON $ repNumKeyboardVkTxt' cmd lst

epilogue1 ::
      (Monad m)
   => D.Handle Vk m
   -> Vk
   -> [VkUpdate]
   -> VkUpdateReplySuccess
   -> m ()
epilogue1 h _ _ rep =
   case _VURS_ts rep of
      Nothing -> return ()
      Just x -> putTimestamp (D.specH h) x

processMessage1 ::
      (Monad m)
   => D.Handle Vk m
   -> Vk
   -> VkMessage
   -> m (Maybe (m H.HTTPRequest))
processMessage1 h _ m
  -- | null atts && maybeText == Nothing = do
   | null atts && isNothing maybeText = do
      D.logError h $
         funcName <> "Unable to send empty message."
      return Nothing
   | otherwise = do
      let (maybePars, toLog) =
             runWriter $ attsToParsVk' atts
      D.logDebug h $ funcName <> "processing vk attachments"
      mapM_ (D.logEntry h) toLog
      S.withMaybe
         maybeText
         (S.withMaybe
             maybePars
             (do
              D.logError
                 h
                 (funcName <>
                  "no text found and unable to send any attachments.")
              return Nothing)
             (return .
              Just . processMessageVk h user maybeText))
         (\_ ->
             let justPars = fromMaybe [] maybePars
              in return $
                 Just
                    (processMessageVk
                        h
                        user
                        maybeText
                        justPars))
  where
    funcName = "vk_processMessage: "
    maybeText = S.emptyToNothing $ _VM_text m
    atts = _VM_attachments m
    user = _VM_from_id m

processMessageVk ::
      (Monad m)
   => D.Handle Vk m
   -> VkUser
   -> Maybe T.Text
   -> H.ParamsList
   -> m H.HTTPRequest
processMessageVk h user maybeText attachmentsEtc = do
   let sc = D.getConstState h
       method = "messages.send"
   randomID <- getRandomID (D.specH h)
   let pars =
          [ unit "user_id" $ _VU_id user
          , mUnit "message" maybeText
          , unit "random_id" randomID
          ] ++
          defaultVkParams sc
   return $
      buildHTTP (vkUrl sc) (method, attachmentsEtc ++ pars)
