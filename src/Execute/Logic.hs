{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Execute.Logic
    ( handleUpdate
    , sendNTimes
    , validateRepNum
    ) where

import qualified App.BotHandler as BotH
import BotTypesClass.ClassTypes (BotClassTypes(..))
import Control.Monad (replicateM)
import Data.Either (partitionEithers)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Environment as Env
import Execute.BotClass (BotClass(..), BotClassUtility(..))
import qualified Execute.Types as ET
import qualified GenericPretty as GP
import qualified HTTP.Types as H
import qualified Stuff as S
import Text.Read (readMaybe)

handleUpdate ::
       (BotClass s, Monad m) => BotH.BotHandler s m -> Upd s -> m ()
handleUpdate h u =
    case defineUpdateType h u of
        ET.ECommand cmd mChat mUser -> handleCommand h cmd mChat mUser
        ET.EMessage m -> handleMessage h m
        ET.ECallback callback -> handleCallback h callback
        ET.EError err -> do
            BotH.logError h "handleUpdate: unexpected update type"
            BotH.logError h $ GP.textPretty err

defineUpdateType ::
       forall s m. (BotClass s)
    => BotH.BotHandler s m
    -> Upd s
    -> ET.Event (Chat s) (User s) (Msg s) (CallbackQuery s)
defineUpdateType h u =
    let mMsg = getMsg @s u
        mText = mMsg >>= getText @s
        mUser = mMsg >>= getUser @s
        mChat = mMsg >>= getChat @s
        mCmd =
            mText >>= S.safeHead . T.words >>= getCmd (BotH.commonEnv h)
        mCallback = getCallbackQuery @s u
        unexpectedValue = getUpdateValue @s u
     in fromMaybe (ET.EError unexpectedValue) $
        asum
            [ ET.ECallback <$> mCallback
            , ET.ECommand <$> mCmd <*> Just mChat <*> Just mUser
            , ET.EMessage <$> mMsg
            ]

getCmd :: Env.Environment -> T.Text -> Maybe ET.Command
getCmd e str
    | str == Env.getHelpCommand e = Just ET.Help
    | str == Env.getSetRepNumCommand e = Just ET.SetRepNum
    | otherwise = Nothing

handleCallback ::
       forall s m. (BotClass s, Monad m)
    => BotH.BotHandler s m
    -> CallbackQuery s
    -> m ()
handleCallback h callback =
    case defineCallbackType @s callback of
        ET.CSetRepNum user mChat n ->
            handleSetRepNum @s h user mChat n
        ET.CError err ->
            BotH.logError h $ "handleCallback: " <> T.pack err

defineCallbackType ::
       forall s. (BotClass s)
    => CallbackQuery s
    -> ET.CallbQuery (User s) (Chat s)
defineCallbackType callback =
    let noCallbackMsg = "No callback data found, unable to respond."
        expectedNumberMsg = "Expected number after \"set\" command."
        unknownCallbackQueryMsg = "Unknown callback query"
        user = getCallbackUser @s callback
        mData = getCallbackData @s callback
        mChat = getCallbackChat @s callback
        eithSetN = do
            dat <- maybe (Left noCallbackMsg) Right mData
            case T.words dat of
                ("set":num:_) ->
                    maybe
                        (Left expectedNumberMsg)
                        Right
                        (readMaybe $ T.unpack num :: Maybe Int)
                _ -> Left unknownCallbackQueryMsg
     in either ET.CError id $
        asum [ET.CSetRepNum user mChat <$> eithSetN]

handleSetRepNum ::
       forall s m. (BotClass s, Monad m)
    => BotH.BotHandler s m
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
            BotH.logError h $
            funcName <> "failed to send message: " <> T.pack x
    BotH.insertUser h user repnum
    eithReqFunc <- sendTextMsg h mChat (Just user) text
    BotH.logInfo h $ funcName <> afterLog
    either
        sendFail
        (sendFixedInfo h $ BotH.sendRepNumMessage h)
        eithReqFunc

handleCommand ::
       (BotClass s, Monad m)
    => BotH.BotHandler s m
    -> ET.Command
    -> Maybe (Chat s)
    -> Maybe (User s)
    -> m ()
handleCommand h cmd mChat mUser =
    case cmd of
        ET.Help -> sendHelp h mChat mUser
        ET.SetRepNum -> sendRepNumButtons h mChat mUser

sendHelp ::
       (BotClass s, Monad m)
    => BotH.BotHandler s m
    -> Maybe (Chat s)
    -> Maybe (User s)
    -> m ()
sendHelp h mChat mUser = do
    let funcName = "sendHelp: "
    eithReqFunc <-
        sendTextMsg h mChat mUser (Env.getHelpMessage $ BotH.commonEnv h)
    BotH.logDebug h $
        funcName <> "sending HTTP request to send help message"
    S.withEither
        eithReqFunc
        (BotH.logError h . (funcName <>) . T.pack)
        (sendFixedInfo h $ BotH.sendHelp h)

minRepNum, maxRepNum :: Int
minRepNum = 1

maxRepNum = 5

sendRepNumButtons ::
       forall s m. (BotClass s, Monad m)
    => BotH.BotHandler s m
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
            (Env.getRepeatQuestion $ BotH.commonEnv h)
    let eithReqFuncKeyboard =
            fmap (H.addParams inlKeyboardPars) eithReqFunc
        inlKeyboardPars =
            repNumKeyboard @s [minRepNum .. maxRepNum] "set"
    BotH.logDebug h $
        funcName <>
        "sending HTTP request to send repeat number buttons"
    S.withEither
        eithReqFuncKeyboard
        (BotH.logError h . (funcName <>) . T.pack)
        (sendFixedInfo h $ BotH.sendKeyboard h)

logEither ::
       (BotClass s, Monad m)
    => BotH.BotHandler s m
    -> (a -> m ())
    -> Either String a
    -> m ()
logEither h = either (BotH.logError h . T.pack)

sendFixedInfo ::
       (BotClass s, Monad m)
    => BotH.BotHandler s m
    -> (a -> m (Either String (Rep s)))
    -> a
    -> m ()
sendFixedInfo h send request = do
    eithResp <- send request
    logEitherResponse h eithResp

logEitherResponse ::
       (BotClass s, Monad m)
    => BotH.BotHandler s m
    -> Either String (Rep s)
    -> m ()
logEitherResponse h = logEither h (logResponse h)

logResponse ::
       forall s m. (BotClass s, Monad m)
    => BotH.BotHandler s m
    -> Rep s
    -> m ()
logResponse h resp =
    let funcName = "logResponse: "
     in if isSuccess @s resp
            then BotH.logInfo h $
                 funcName <> "Ok: " <> GP.textPretty resp
            else BotH.logError h $
                 funcName <> "Failed request: " <> GP.textPretty resp

handleMessage ::
       forall s m. (BotClass s, Monad m)
    => BotH.BotHandler s m
    -> Msg s
    -> m ()
handleMessage h m = do
    let funcName = "handleMessage: "
        maybeUser = getUser @s m
    mReqFunc <- processMessage h m
    S.withMaybe mReqFunc (pure ()) $ \req -> do
        BotH.logDebug h $ funcName <> "sending some copies of message"
        BotH.logDebug h $ GP.textPretty m
        sendNTimes @s h maybeUser req

sendNTimes ::
       (BotClass s, Monad m)
    => BotH.BotHandler s m
    -> Maybe (User s)
    -> m H.HTTPRequest
    -> m ()
sendNTimes h maybeUser req = do
    let funcName = "sendNTimes: "
        defaultRepNum = Env.getDefaultRepNum $ BotH.commonEnv h
    repNraw <- BotH.findWithDefault h defaultRepNum maybeUser
    let repN = validateRepNum repNraw
    BotH.logDebug h $
        funcName <> "sending message " <> S.showT repN <> " times."
    reqs <- replicateM repN req
    eithRespList <- mapM (BotH.sendEcho h) reqs
    let (errs, resps) = partitionEithers eithRespList
    mapM_ (BotH.logError h . T.pack) errs
    mapM_ (logResponse h) resps

validateRepNum :: Int -> Int
validateRepNum x
    | x > maxRepNum = maxRepNum
    | x < minRepNum = minRepNum
    | otherwise = x
