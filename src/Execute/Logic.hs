{-# LANGUAGE AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables #-}
module Execute.Logic where

import Data.Maybe
import qualified App.Handle as D
import BotClass.Class
import BotClass.ClassTypes
import Control.Monad (replicateM)
import Data.Either (partitionEithers)
import Data.Foldable (asum)
import qualified Data.Text as T
import qualified GenericPretty as GP
import qualified HTTPRequests as H
import qualified Stuff as S
import Text.Read (readMaybe)
import Types

handleUpdate ::
      (BotClass s, Monad m)
   => D.Handle s m
   -> Upd s
   -> m ()
handleUpdate h u = do
   case defineUpdateType h u of
      ECommand cmd mChat mUser ->
         handleCommand h cmd mChat mUser
      EMessage m -> handleMessage h m
      ECallback callback -> handleCallback h callback
      EError err -> do
         D.logError h "handleUpdate: unexpected update type"
         D.logError h $ GP.defaultPrettyT err

type EventT s
    = Event (Chat s) (User s) (Msg s) (CallbackQuery s)

defineUpdateType ::
      forall s m. (BotClass s) => D.Handle s m -> Upd s -> Event (Chat s) (User s) (Msg s) (CallbackQuery s)
defineUpdateType h u =
   let mMsg = getMsg @s u
       mText = mMsg >>= getText @s
       mUser = mMsg >>= getUser @s
       mChat = mMsg >>= getChat @s
       mCmd =
          mText >>= S.safeHead . T.words >>=
          getCmd (D.commonEnv h)
       mCallback = getCallbackQuery @s u
       unexpectedValue = getUpdateValue @s u
    in
       fromMaybe (EError unexpectedValue) $
       asum
          [ ECallback <$> mCallback
          , ECommand <$> mCmd <*> Just mChat <*> Just mUser
          , EMessage <$> mMsg
          ]

getCmd :: EnvironmentCommon -> T.Text -> Maybe Command
getCmd e str
   | str == helpCommand e = Just Help
   | str == setRepNumCommand e = Just SetRepNum
   | otherwise = Nothing

handleCallback ::
      forall s m. (BotClass s, Monad m)
   => D.Handle s m
   -> CallbackQuery s
   -> m ()
handleCallback h callback =
   case defineCallbackType @s callback of
      CSetRepNum user mChat n ->
         handleSetRepNum @s h user mChat n
      CError err ->
         D.logError h $ "handleCallback: " <> T.pack err

defineCallbackType ::
      forall s. (BotClass s)
   => CallbackQuery s
   -> CallbQuery (User s) (Chat s)
defineCallbackType callback =
   let user = getCallbackUser @s callback
       mData = getCallbackData @s callback
       mChat = getCallbackChat @s callback
       eithSetN = do
          dat <-
             maybe
                (Left
                    "No callback data found, unable to respond.")
                Right
                mData
          case T.words dat of
             ("set":num:_) ->
                maybe
                   (Left
                       "Expected number after \"set\" command.")
                   Right
                   (readMaybe $ T.unpack num :: Maybe Int)
             _ -> Left "Unknown callback query"
    in either CError id $
       asum [CSetRepNum user mChat <$> eithSetN]

handleSetRepNum ::
      forall s m. (BotClass s, Monad m)
   => D.Handle s m
   -> User s
   -> Maybe (Chat s)
   -> Int
   -> m ()
handleSetRepNum h user mChat repnum = do
   let funcName = "handleSetRepNum: "
       text =
          "Now every your message will be repeated " <>
          S.showT repnum <> " times."
       afterLog =
          "User with ID = " <>
          getUserID @s user <>
          " set " <> S.showT repnum <> " repeats."
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
      (BotClass s, Monad m)
   => D.Handle s m
   -> Command
   -> Maybe (Chat s)
   -> Maybe (User s)
   -> m ()
handleCommand h cmd mChat mUser =
   case cmd of
      Help -> sendHelp h mChat mUser
      SetRepNum -> sendRepNumButtons h mChat mUser

sendHelp ::
      (BotClass s, Monad m)
   => D.Handle s m
   -> Maybe (Chat s)
   -> Maybe (User s)
   -> m ()
sendHelp h mChat mUser = do
   let funcName = "sendHelp: "
   eithReqFunc <-
      sendTextMsg h mChat mUser (helpMsg $ D.commonEnv h)
   D.logDebug h $
      funcName <>
      "sending HTTP request to send help message"
   S.withEither
      eithReqFunc
      (D.logError h . (funcName <>) . T.pack)
      (sendFixedInfo h $ D.sendHelp h)

minRepNum, maxRepNum :: Int
minRepNum = 1

maxRepNum = 5

sendRepNumButtons ::
      forall s m. (BotClass s, Monad m)
   => D.Handle s m
   -> Maybe (Chat s)
   -> Maybe (User s)
   -> m ()
sendRepNumButtons h mChat mUser = do
   let funcName = "sendRepNumButtons: "
   eithReqFunc <-
      sendTextMsg
         h
         mChat
         mUser
         (repQuestion $ D.commonEnv h)
   let eithReqFuncKeyboard =
          fmap (H.addParams inlKeyboardPars) eithReqFunc
       inlKeyboardPars =
          repNumKeyboard @s [minRepNum .. maxRepNum] "set"
   D.logDebug h $
      funcName <>
      "sending HTTP request to send repeat number buttons"
   S.withEither
      eithReqFuncKeyboard
      (D.logError h . (funcName <>) . T.pack)
      (sendFixedInfo h $ D.sendKeyboard h)

logEither ::
      (BotClass s, Monad m)
   => D.Handle s m
   -> (a -> m ())
   -> Either String a
   -> m ()
logEither h f = do
   either (D.logError h . T.pack) f

sendFixedInfo ::
      (BotClass s, Monad m)
   => D.Handle s m
   -> (a -> m (Either String (Rep s)))
   -> a
   -> m ()
sendFixedInfo h send request = do
   eithResp <- send request
   logEitherResponse h eithResp

logEitherResponse ::
      (BotClass s, Monad m)
   => D.Handle s m
   -> Either String (Rep s)
   -> m ()
logEitherResponse h = logEither h (logResponse h)

logResponse ::
      forall s m. (BotClass s, Monad m)
   => D.Handle s m
   -> Rep s
   -> m ()
logResponse h resp =
   let funcName = "logResponse: "
    in if isSuccess @s resp
          then D.logInfo h $
               funcName <> "Ok: " <> GP.textPretty resp
          else D.logError h $
               funcName <>
               "Failed request: " <> GP.textPretty resp

handleMessage ::
      forall s m. (BotClass s, Monad m)
   => D.Handle s m
   -> Msg s
   -> m ()
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
      (BotClass s, Monad m)
   => D.Handle s m
   -> Maybe (User s)
   -> m H.HTTPRequest
   -> m ()
sendNTimes h maybeUser req = do
   let funcName = "sendNTimes: "
       defaultRepNum = repNum $ D.commonEnv h
   repNraw <- D.findWithDefault h defaultRepNum maybeUser
   let repN = validateRepNum repNraw
   D.logDebug h $
      funcName <>
      "sending message " <> S.showT repN <> " times."
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
