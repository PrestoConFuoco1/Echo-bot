{-# LANGUAGE TypeFamilies, RecordWildCards, DataKinds, TypeApplications #-}

module BotClass.BotTeleInstance where

import qualified App.Handle as D
import BotClass.Class
import BotClass.ClassTypesTeleInstance
import qualified Control.Monad.Catch as C (throwM)
import qualified Data.Aeson.Types as AeT
   ( parseEither
   , parseJSON
   , toJSON
   )
import Data.Bifunctor (first)
import Data.Foldable (asum)
import qualified Data.Text as T (pack)
import qualified Data.Text.Lazy as T (fromStrict)
import qualified Exceptions as Ex
import qualified Execute.Logic as E (sendNTimes)
import qualified GenericPretty as GP
import HTTPRequests as H
import qualified Stuff as S
import Telegram
import Types

instance BotClassUtility 'Telegram where
   getResult = Just . _TURS_result
   getMsg TlUpdate {..} =
      case _TU_event of
         TEMsg m -> Just m
         _ -> Nothing
   getUpdateValue = _TU_value
   getChat = Just . _TM_chat
   getUser = _TM_from
   getText = _TM_text
   getUserID = T.pack . show . _TUs_id
   getCallbackQuery TlUpdate {..} =
      case _TU_event of
         TECallback cb -> Just cb
         _ -> Nothing
   getCallbackUser = _TCB_from
   getCallbackData = _TCB_data
   getCallbackChat c =
      _TM_chat <$> _TCB_message c

instance BotClass 'Telegram
                         where
   takesJSON = tlTakesJSON
   getUpdatesRequest h = do
      let tout = timeout $ D.commonEnv h
          url = tlUrl $ D.getConstState h
          req uid =
             H.Req
                H.GET
                (url <> "getUpdates")
                [unit "offset" uid, unit "timeout" tout]
      curUpdID <- getUpdateID (D.specH h)
      pure $ req curUpdID
   isSuccess = _TR_ok
   handleFailedUpdatesRequest h err = do
      let funcName = "tl_handleFailedUpdatesRequest: "
      D.logError h $ funcName <> "got telegram error"
      D.logError h $ GP.defaultPrettyT err
      D.logError h $
         funcName <> "unable to handle any telegram errors"
      C.throwM Ex.UnableToHandleError
   parseUpdatesValueList rep = do
      res <-
         maybe (Left "Couldn't parse update list") Right $
         getResult @Telegram rep
      AeT.parseEither AeT.parseJSON res
   parseUpdate = AeT.parseEither AeT.parseJSON
   repNumKeyboard lst cmd = [unit "reply_markup" obj]
     where
       obj = AeT.toJSON $ repNumKeyboardTele' cmd lst
   sendTextMsg _ Nothing _ _ =
      pure $
      Left
         "Telegram: no chat supplied, unable to send messages to users"
   sendTextMsg h (Just c) _ text =
      let url = tlUrl $ D.getConstState h
       in pure $
          Right $
          buildHTTP
             url
             ( "sendMessage"
             , [unit "chat_id" $ _TC_id c, unit "text" text])
   epilogue _ [] _ = pure ()
   epilogue h us _ = do
      let funcName = "tl_epilogue: "
          newUpdateID = maximum (map _TU_update_id us) + 1
      putUpdateID (D.specH h) newUpdateID
      mediaGroups <- getMediaGroups (D.specH h)
      D.logDebug h $
         funcName <>
         "ready to process some media groups, if any"
      D.logDebug h $ GP.defaultPrettyT mediaGroups
      mapM_ (sendMediaGroup h) mediaGroups
      purgeMediaGroups (D.specH h)
   processMessage = processMessage1

processMessage1 ::
      (Monad m)
   => D.Handle 'Telegram m
   -> TlMessage
   -> m (Maybe (m H.HTTPRequest))
processMessage1 h m =
   if isMediaGroup m
      then processMediaGroup h m >> pure Nothing
      else S.withEither
              sendMessage
              (\e -> do
                  D.logError h $ funcName <> T.pack e
                  pure Nothing)
              (pure . Just)
  where
    funcName = "tl_processMessage: "
    sendMessage =
       let eithMethodParams = sendMessageTele m
           url = tlUrl $ D.getConstState h
        in pure . buildHTTP url <$>
           eithMethodParams

processMediaGroup ::
      (Monad m) => D.Handle 'Telegram m -> TlMessage -> m ()
processMediaGroup h m =
   let funcName = "processMediaGroup: "
       chat = _TM_chat m
       mUser = _TM_from m
       mMediaGroupID = _TM_media_group_id m
       mMediaGroupIdent =
          TlMediaGroupIdentifier chat mUser <$>
          mMediaGroupID
       mMediaGroupUnit = maybeMediaGroupUnit m
       mAction =
          asum
             [ insertMediaGroupUnit (D.specH h) <$>
               mMediaGroupIdent <*>
               mMediaGroupUnit
             ]
       processFail = do
          D.logWarning h $
             funcName <>
             "failed to process media group. Message:"
          D.logWarning h $ GP.textPretty m
    in S.withMaybe mAction processFail $ \action -> do
          D.logDebug h $
             funcName <> "processing media group"
          action

sendMediaGroup ::
      (Monad m)
   => D.Handle 'Telegram m
   -> TlMediaGroupPair
   -> m ()
sendMediaGroup h (TlMediaGroupPair ident items) = do
   let chat = _TMGI_chat ident
       mUser = _TMGI_user ident
       items' = reverse items
       method = "sendMediaGroup"
       url = tlUrl $ D.getConstState h
       mCaption = S.safeHead items >>= photoVideoCaption
       pars =
          [ unit "chat_id" $ _TC_id chat
          , unit "media" $ AeT.toJSON items'
          , mUnit "caption" mCaption
          ]
       req = buildHTTP url (method, pars)
   E.sendNTimes h mUser (pure req)
