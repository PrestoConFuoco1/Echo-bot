{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Execute.Telegram () where

import qualified App.Handle as D
import qualified App.Handle.Telegram as HT
import BotTypesClass.TelegramInstance ()
import qualified Control.Monad.Catch as C (MonadThrow, throwM)
import qualified Data.Aeson.Types as AeT
  ( Value,
    parseEither,
    parseJSON,
    toJSON,
  )
import Data.Foldable (asum)
import qualified Data.Text as T (Text, pack)
import qualified Environment as Env
import qualified Exceptions as Ex
import Execute.BotClass
import qualified Execute.Logic as E (sendNTimes)
import qualified GenericPretty as GP
import HTTP.Types as H
import qualified Messenger as M
import qualified Stuff as S
import Telegram

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
  getUpdatesRequest = getUpdatesRequestTl
  isSuccess = replyOk
  handleFailedUpdatesRequest = handleFailedUpdatesRequestTl
  parseUpdatesValueList = parseUpdatesValueListTl
  parseUpdate = parseUpdateTl
  repNumKeyboard = repNumKeyboardTl
  sendTextMsg = sendTextMsgTl
  processMessage = processMessageTl
  epilogue = epilogueTl

getUpdatesRequestTl :: (Monad m) => D.BotHandler 'M.Telegram m -> m HTTPRequest
getUpdatesRequestTl h = do
  let tout = Env.getDefaultTimeout $ D.commonEnv h
      url = HT.stcUrl $ D.getConstState h
      req uid =
        H.Req
          H.GET
          (url <> "getUpdates")
          [unit "offset" uid, unit "timeout" tout]
  curUpdID <- HT.getUpdateID (D.specH h)
  pure $ req curUpdID

handleFailedUpdatesRequestTl :: (C.MonadThrow m) => D.BotHandler 'M.Telegram m -> TlUpdateReplyError -> m b
handleFailedUpdatesRequestTl h err = do
  let funcName = "tl_handleFailedUpdatesRequest: "
  D.logError h $ funcName <> "got telegram error"
  D.logError h $ GP.defaultPrettyT err
  D.logError h $
    funcName <> "unable to handle any telegram errors"
  C.throwM Ex.UnableToHandleError

parseUpdatesValueListTl :: TlUpdateReplySuccess -> Either String [AeT.Value]
parseUpdatesValueListTl rep = do
  res <-
    maybe (Left "Couldn't parse update list") Right $
      getResult @'M.Telegram rep
  AeT.parseEither AeT.parseJSON res

parseUpdateTl :: AeT.Value -> Either String TlUpdate
parseUpdateTl = AeT.parseEither AeT.parseJSON

repNumKeyboardTl :: [Int] -> T.Text -> [H.ParamsUnit]
repNumKeyboardTl lst cmd = [unit "reply_markup" obj]
  where
    obj = AeT.toJSON $ repNumKeyboardTele cmd lst

sendTextMsgTl :: (Monad m) => D.BotHandler 'M.Telegram m -> Maybe TlChat -> Maybe TlUser -> T.Text -> m (Either String H.HTTPRequest)
sendTextMsgTl _ Nothing _ _ =
  pure $
    Left
      "M.Telegram: no chat supplied, unable to send messages to users"
sendTextMsgTl h (Just c) _ text =
  let url = HT.stcUrl $ D.getConstState h
   in pure $
        Right $
          buildHTTP
            url
            ( "sendMessage",
              [unit "chat_id" $ chatID c, unit "text" text]
            )

epilogueTl :: (Monad m) => D.BotHandler 'M.Telegram m -> [TlUpdate] -> TlUpdateReplySuccess -> m ()
epilogueTl _ [] _ = pure ()
epilogueTl h us _ = do
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

processMessageTl ::
  (Monad m) =>
  D.BotHandler 'M.Telegram m ->
  TlMessage ->
  m (Maybe (m H.HTTPRequest))
processMessageTl h m =
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
      let eithMethodParams = echoMessageTele m
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
