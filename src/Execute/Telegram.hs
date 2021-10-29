{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Execute.Telegram where

import qualified App.Handle as D
import qualified App.Handle.Telegram as HT
import BotTypesClass.TelegramInstance ()
import qualified Control.Monad.Catch as C (throwM)
import qualified Data.Aeson.Types as AeT
  ( parseEither,
    parseJSON,
    toJSON,
  )
import Data.Foldable (asum)
import qualified Data.Text as T (pack)
import qualified Exceptions as Ex
import Execute.BotClass
import qualified Execute.Logic as E (sendNTimes)
import qualified GenericPretty as GP
import HTTPTypes as H
import qualified Stuff as S
import Telegram
import qualified Messenger as M
import qualified Environment as Env

instance BotClassUtility 'M.Telegram where
  getResult = Just . replysuccessResult
  getMsg TlUpdate {..} =
    case updateEvent of
      TEMsg m -> Just m
      _ -> Nothing
  getUpdateValue = updateValue
  getChat = Just . messageChat
  getUser = messageFrom
  getText = messageText
  getUserID = T.pack . show . userID
  getCallbackQuery TlUpdate {..} =
    case updateEvent of
      TECallback cb -> Just cb
      _ -> Nothing
  getCallbackUser = callbackFrom
  getCallbackData = callbackData
  getCallbackChat c =
    messageChat <$> callbackMessage c

instance BotClass 'M.Telegram where
  takesJSON = tlTakesJSON
  getUpdatesRequest h = do
    let tout = Env.getDefaultTimeout $ D.commonEnv h
        url = HT.stcUrl $ D.getConstState h
        req uid =
          H.Req
            H.GET
            (url <> "getUpdates")
            [unit "offset" uid, unit "timeout" tout]
    curUpdID <- HT.getUpdateID (D.specH h)
    pure $ req curUpdID
  isSuccess = replyOk
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
        getResult @'M.Telegram rep
    AeT.parseEither AeT.parseJSON res
  parseUpdate = AeT.parseEither AeT.parseJSON
  repNumKeyboard lst cmd = [unit "reply_markup" obj]
    where
      obj = AeT.toJSON $ repNumKeyboardTele cmd lst
  sendTextMsg _ Nothing _ _ =
    pure $
      Left
        "M.Telegram: no chat supplied, unable to send messages to users"
  sendTextMsg h (Just c) _ text =
    let url = HT.stcUrl $ D.getConstState h
     in pure $
          Right $
            buildHTTP
              url
              ( "sendMessage",
                [unit "chat_id" $ chatID c, unit "text" text]
              )
  epilogue _ [] _ = pure ()
  epilogue h us _ = do
    let funcName = "tl_epilogue: "
        newUpdateID = maximum (map updateUpdateID us) + 1
    HT.putUpdateID (D.specH h) newUpdateID
    stmMediaGroups <- HT.getMediaGroups (D.specH h)
    D.logDebug h $
      funcName
        <> "ready to process some media groups, if any"
    D.logDebug h $ GP.defaultPrettyT stmMediaGroups
    mapM_ (sendMediaGroup h) stmMediaGroups
    HT.purgeMediaGroups (D.specH h)
  processMessage = processMessage1

processMessage1 ::
  (Monad m) =>
  D.BotHandler 'M.Telegram m ->
  TlMessage ->
  m (Maybe (m H.HTTPRequest))
processMessage1 h m =
  if isMediaGroup m
    then processMediaGroup h m >> pure Nothing
    else
      S.withEither
        sendMessage
        ( \e -> do
            D.logError h $ funcName <> T.pack e
            pure Nothing
        )
        (pure . Just)
  where
    funcName = "tl_processMessage: "
    sendMessage =
      let eithMethodParams = sendMessageTele m
          url = HT.stcUrl $ D.getConstState h
       in pure . buildHTTP url
            <$> eithMethodParams

processMediaGroup ::
  (Monad m) => D.BotHandler 'M.Telegram m -> TlMessage -> m ()
processMediaGroup h m =
  let funcName = "processMediaGroup: "
      chat = messageChat m
      mUser = messageFrom m
      mMediaGroupID = messageMediaGroupID m
      mMediaGroupIdent =
        TlMediaGroupIdentifier chat mUser
          <$> mMediaGroupID
      mMediaGroupUnit = maybeMediaGroupUnit m
      mAction =
        asum
          [ HT.insertMediaGroupUnit (D.specH h)
              <$> mMediaGroupIdent
              <*> mMediaGroupUnit
          ]
      processFail = do
        D.logWarning h $
          funcName
            <> "failed to process media group. Message:"
        D.logWarning h $ GP.textPretty m
   in S.withMaybe mAction processFail $ \action -> do
        D.logDebug h $
          funcName <> "processing media group"
        action

sendMediaGroup ::
  (Monad m) =>
  D.BotHandler 'M.Telegram m ->
  TlMediaGroupPair ->
  m ()
sendMediaGroup h (TlMediaGroupPair ident items) = do
  let chat = tmgidChat ident
      mUser = tmgidUser ident
      reversedItems = reverse items
      method = "sendMediaGroup"
      url = HT.stcUrl $ D.getConstState h
      mCaption = S.safeHead items >>= photoVideoCaption
      pars =
        [ unit "chat_id" $ chatID chat,
          unit "media" $ AeT.toJSON reversedItems,
          mUnit "caption" mCaption
        ]
      req = buildHTTP url (method, pars)
  E.sendNTimes h mUser (pure req)
