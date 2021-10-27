{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Execute.Logic where

import qualified App.Handle as D
import BotClass (BotClass (..), BotClassUtility (..))
import BotTypesClass.ClassTypes (BotClassTypes (..))
import Control.Monad (replicateM)
import Data.Either (partitionEithers)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified GenericPretty as GP
import qualified HTTPTypes as H
import qualified Stuff as S
import Text.Read (readMaybe)
import qualified Types as Y

handleUpdate ::
  (BotClass s, Monad m) =>
  D.BotHandler s m ->
  Upd s ->
  m ()
handleUpdate h u = do
  case defineUpdateType h u of
    Y.ECommand cmd mChat mUser ->
      handleCommand h cmd mChat mUser
    Y.EMessage m -> handleMessage h m
    Y.ECallback callback -> handleCallback h callback
    Y.EError err -> do
      D.logError h "handleUpdate: unexpected update type"
      D.logError h $ GP.defaultPrettyT err

type EventT s =
  Y.Event (Chat s) (User s) (Msg s) (CallbackQuery s)

defineUpdateType ::
  forall s m. (BotClass s) => D.BotHandler s m -> Upd s -> Y.Event (Chat s) (User s) (Msg s) (CallbackQuery s)
defineUpdateType h u =
  let mMsg = getMsg @s u
      mText = mMsg >>= getText @s
      mUser = mMsg >>= getUser @s
      mChat = mMsg >>= getChat @s
      mCmd =
        mText >>= S.safeHead . T.words
          >>= getCmd (D.commonEnv h)
      mCallback = getCallbackQuery @s u
      unexpectedValue = getUpdateValue @s u
   in fromMaybe (Y.EError unexpectedValue) $
        asum
          [ Y.ECallback <$> mCallback,
            Y.ECommand <$> mCmd <*> Just mChat <*> Just mUser,
            Y.EMessage <$> mMsg
          ]

getCmd :: Y.EnvironmentCommon -> T.Text -> Maybe Y.Command
getCmd e str
  | str == Y.getHelpCommand e = Just Y.Help
  | str == Y.getSetRepNumCommand e = Just Y.SetRepNum
  | otherwise = Nothing

handleCallback ::
  forall s m.
  (BotClass s, Monad m) =>
  D.BotHandler s m ->
  CallbackQuery s ->
  m ()
handleCallback h callback =
  case defineCallbackType @s callback of
    Y.CSetRepNum user mChat n ->
      handleSetRepNum @s h user mChat n
    Y.CError err ->
      D.logError h $ "handleCallback: " <> T.pack err

defineCallbackType ::
  forall s.
  (BotClass s) =>
  CallbackQuery s ->
  Y.CallbQuery (User s) (Chat s)
defineCallbackType callback =
  let user = getCallbackUser @s callback
      mData = getCallbackData @s callback
      mChat = getCallbackChat @s callback
      eithSetN = do
        dat <-
          maybe
            ( Left
                "No callback data found, unable to respond."
            )
            Right
            mData
        case T.words dat of
          ("set" : num : _) ->
            maybe
              ( Left
                  "Expected number after \"set\" command."
              )
              Right
              (readMaybe $ T.unpack num :: Maybe Int)
          _ -> Left "Unknown callback query"
   in either Y.CError id $
        asum [Y.CSetRepNum user mChat <$> eithSetN]

handleSetRepNum ::
  forall s m.
  (BotClass s, Monad m) =>
  D.BotHandler s m ->
  User s ->
  Maybe (Chat s) ->
  Int ->
  m ()
handleSetRepNum h user mChat repnum = do
  let funcName = "handleSetRepNum: "
      text =
        "Now every your message will be repeated "
          <> S.showT repnum
          <> " times."
      afterLog =
        "User with ID = "
          <> getUserID @s user
          <> " set "
          <> S.showT repnum
          <> " repeats."
      sendFail x =
        D.logError h $
          funcName <> "failed to send message: " <> T.pack x
  D.insertUser h user repnum
  eithReqFunc <- sendTextMsg h mChat (Just user) text
  D.logInfo h $ funcName <> afterLog
  either
    sendFail
    (sendFixedInfo h $ D.sendRepNumMessage h)
    eithReqFunc

handleCommand ::
  (BotClass s, Monad m) =>
  D.BotHandler s m ->
  Y.Command ->
  Maybe (Chat s) ->
  Maybe (User s) ->
  m ()
handleCommand h cmd mChat mUser =
  case cmd of
    Y.Help -> sendHelp h mChat mUser
    Y.SetRepNum -> sendRepNumButtons h mChat mUser

sendHelp ::
  (BotClass s, Monad m) =>
  D.BotHandler s m ->
  Maybe (Chat s) ->
  Maybe (User s) ->
  m ()
sendHelp h mChat mUser = do
  let funcName = "sendHelp: "
  eithReqFunc <-
    sendTextMsg h mChat mUser (Y.getHelpMessage $ D.commonEnv h)
  D.logDebug h $
    funcName
      <> "sending HTTP request to send help message"
  S.withEither
    eithReqFunc
    (D.logError h . (funcName <>) . T.pack)
    (sendFixedInfo h $ D.sendHelp h)

minRepNum, maxRepNum :: Int
minRepNum = 1
maxRepNum = 5

sendRepNumButtons ::
  forall s m.
  (BotClass s, Monad m) =>
  D.BotHandler s m ->
  Maybe (Chat s) ->
  Maybe (User s) ->
  m ()
sendRepNumButtons h mChat mUser = do
  let funcName = "sendRepNumButtons: "
  eithReqFunc <-
    sendTextMsg
      h
      mChat
      mUser
      (Y.getRepeatQuestion $ D.commonEnv h)
  let eithReqFuncKeyboard =
        fmap (H.addParams inlKeyboardPars) eithReqFunc
      inlKeyboardPars =
        repNumKeyboard @s [minRepNum .. maxRepNum] "set"
  D.logDebug h $
    funcName
      <> "sending HTTP request to send repeat number buttons"
  S.withEither
    eithReqFuncKeyboard
    (D.logError h . (funcName <>) . T.pack)
    (sendFixedInfo h $ D.sendKeyboard h)

logEither ::
  (BotClass s, Monad m) =>
  D.BotHandler s m ->
  (a -> m ()) ->
  Either String a ->
  m ()
logEither h f = do
  either (D.logError h . T.pack) f

sendFixedInfo ::
  (BotClass s, Monad m) =>
  D.BotHandler s m ->
  (a -> m (Either String (Rep s))) ->
  a ->
  m ()
sendFixedInfo h send request = do
  eithResp <- send request
  logEitherResponse h eithResp

logEitherResponse ::
  (BotClass s, Monad m) =>
  D.BotHandler s m ->
  Either String (Rep s) ->
  m ()
logEitherResponse h = logEither h (logResponse h)

logResponse ::
  forall s m.
  (BotClass s, Monad m) =>
  D.BotHandler s m ->
  Rep s ->
  m ()
logResponse h resp =
  let funcName = "logResponse: "
   in if isSuccess @s resp
        then
          D.logInfo h $
            funcName <> "Ok: " <> GP.textPretty resp
        else
          D.logError h $
            funcName
              <> "Failed request: "
              <> GP.textPretty resp

handleMessage ::
  forall s m.
  (BotClass s, Monad m) =>
  D.BotHandler s m ->
  Msg s ->
  m ()
handleMessage h m = do
  let funcName = "handleMessage: "
      maybeUser = getUser @s m
  mReqFunc <- processMessage h m
  S.withMaybe mReqFunc (pure ()) $ \req -> do
    D.logDebug h $
      funcName <> "sending some copies of message"
    D.logDebug h $ T.pack $ GP.defaultPretty m
    sendNTimes @s h maybeUser req

sendNTimes ::
  (BotClass s, Monad m) =>
  D.BotHandler s m ->
  Maybe (User s) ->
  m H.HTTPRequest ->
  m ()
sendNTimes h maybeUser req = do
  let funcName = "sendNTimes: "
      defaultRepNum = Y.getDefaultRepNum $ D.commonEnv h
  repNraw <- D.findWithDefault h defaultRepNum maybeUser
  let repN = validateRepNum repNraw
  D.logDebug h $
    funcName
      <> "sending message "
      <> S.showT repN
      <> " times."
  reqs <- replicateM repN req
  eithRespList <- mapM (D.sendEcho h) reqs
  let (errs, resps) = partitionEithers eithRespList
  mapM_ (D.logError h . T.pack) errs
  mapM_ (logResponse h) resps

validateRepNum :: Int -> Int
validateRepNum x
  | x > maxRepNum = maxRepNum
  | x < minRepNum = minRepNum
  | otherwise = x
