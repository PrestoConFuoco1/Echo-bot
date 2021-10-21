module Execute.Logic where


import qualified App.Handle as D
import BotClass.ClassTypes
import BotClass.Class
import Types
import qualified Data.Text as T
import qualified GenericPretty as GP
import qualified Stuff as S
import Data.Foldable (asum)
import qualified HTTPRequests as H
import Text.Read (readMaybe)
import Control.Monad (replicateM)
import Data.Either (partitionEithers)


handleUpdate :: (BotClass s, Monad m) =>
    D.Handle s m -> s -> Upd s -> m ()
handleUpdate h s u = do
  case defineUpdateType h s u of
    ECommand cmd mChat mUser -> handleCommand h s cmd mChat mUser
    EMessage m -> handleMessage h s m
    ECallback callback -> handleCallback h s callback
    EError err -> do
        D.logError h $ "handleUpdate: unexpected update type"
        D.logError h $ GP.defaultPrettyT err

type EventT s = Event (Chat s) (User s) (Msg s) (CallbackQuery s)

defineUpdateType ::
    (BotClass s) => D.Handle s m -> s -> Upd s -> EventT s
defineUpdateType h s u =
    let mMsg = getMsg s u
        mText = mMsg >>= getText s
        mUser = mMsg >>= getUser s
        mChat = mMsg >>= getChat s
        mCmd = mText >>= S.safeHead . T.words
            >>= (getCmd $ D.commonEnv h)
        mCallback = getCallbackQuery s u
        unexpectedValue = getUpdateValue s u
    in  maybe (EError unexpectedValue) id $
          asum  [
                ECallback <$> mCallback,
                ECommand <$> mCmd <*> Just mChat <*> Just mUser,
                EMessage <$> mMsg
                ]

getCmd :: EnvironmentCommon -> T.Text -> Maybe Command
getCmd e str
    | str == helpCommand e = Just Help
    | str == setRepNumCommand e = Just SetRepNum
    | otherwise = Nothing

handleCallback :: (BotClass s, Monad m) =>
    D.Handle s m -> s -> CallbackQuery s -> m ()
handleCallback h s callback =
    case defineCallbackType s callback of
        CSetRepNum user mChat n -> handleSetRepNum h s user mChat n
        CError err -> D.logError h $ "handleCallback: " <> T.pack err

defineCallbackType :: (BotClass s) =>
    s -> CallbackQuery s -> CallbQuery (User s) (Chat s)
defineCallbackType s callback =
    let user  = getCallbackUser s callback
        mData = getCallbackData s callback
        mChat = getCallbackChat s callback
        eithSetN = do
            dat <- maybe
                (Left "No callback data found, unable to respond.") Right mData
            repNumDat <- case T.words dat of
                ("set" : num : _) ->
                    maybe (Left "Expected number after \"set\" command.")
                    Right (readMaybe $ T.unpack num :: Maybe Int)
                _ -> Left "Unknown callback query"
            return repNumDat
    in  either CError id $
          asum [ CSetRepNum user mChat <$> eithSetN ]



handleSetRepNum :: (BotClass s, Monad m) =>
    D.Handle s m -> s -> User s -> Maybe (Chat s) -> Int -> m ()
handleSetRepNum h s user mChat repnum = do
    let 
        funcName = "handleSetRepNum: "
        text = "Now every your message will be repeated "
            <> S.showT repnum <> " times."
        afterLog = "User with ID = " <> getUserID s user
            <> " set " <> S.showT repnum <> " repeats."
        sendFail x = D.logError h $ funcName <> "failed to send message: " <> T.pack x
    D.insertUser h user repnum
    eithReqFunc <- sendTextMsg h s mChat (Just user) text
    D.logInfo h $ funcName <> afterLog
    either sendFail (sendFixedInfo h s $ D.sendRepNumMessage h) eithReqFunc

handleCommand :: (BotClass s, Monad m) =>
    D.Handle s m -> s -> Command -> Maybe (Chat s) -> Maybe (User s) -> m ()
handleCommand h s cmd mChat mUser =
  case cmd of 
    Help -> sendHelp h s mChat mUser
    SetRepNum -> sendRepNumButtons h s mChat mUser 

sendHelp :: (BotClass s, Monad m) =>
    D.Handle s m -> s -> Maybe (Chat s) -> Maybe (User s) -> m ()
sendHelp h s mChat mUser = do
    let funcName = "sendHelp: "
    eithReqFunc <- sendTextMsg h s mChat mUser (helpMsg $ D.commonEnv h)
    D.logDebug h $ funcName <> "sending HTTP request to send help message"
    S.withEither eithReqFunc
        (D.logError h . (funcName <>) . T.pack)
        (sendFixedInfo h s $ D.sendHelp h)


minRepNum, maxRepNum :: Int
minRepNum = 1
maxRepNum = 5 

sendRepNumButtons :: (BotClass s, Monad m) =>
    D.Handle s m -> s -> Maybe (Chat s) -> Maybe (User s) -> m ()
sendRepNumButtons h s mChat mUser = do
    let funcName = "sendRepNumButtons: "
    eithReqFunc <- sendTextMsg h s mChat mUser (repQuestion $ D.commonEnv h)
    let eithReqFuncKeyboard = fmap (H.addParams inlKeyboardPars) eithReqFunc
        inlKeyboardPars = repNumKeyboard s [minRepNum..maxRepNum] "set"
    D.logDebug h $ funcName <> "sending HTTP request to send repeat number buttons"
    S.withEither eithReqFuncKeyboard
        (D.logError h . (funcName <>) . T.pack)
        (sendFixedInfo h s $ D.sendKeyboard h)

logEither :: (BotClass s, Monad m) =>
    D.Handle s m -> s -> (a -> m ()) -> Either String a -> m ()
logEither h _ f = do
    either (D.logError h . T.pack) f

sendFixedInfo :: (BotClass s, Monad m) =>
    D.Handle s m -> s -> (a -> m (Either String (Rep s))) -> a -> m ()
sendFixedInfo h s send request = do
    eithResp <- send request
    logEitherResponse h s eithResp



logEitherResponse :: (BotClass s, Monad m) => D.Handle s m -> s -> Either String (Rep s) -> m ()
logEitherResponse h s = logEither h s (logResponse h s)

logResponse :: (BotClass s, Monad m) => D.Handle s m -> s -> Rep s -> m ()
logResponse h s resp =
    let funcName = "logResponse: "
    in if isSuccess s resp
        then D.logInfo h $ funcName <> "Ok: " <> GP.textPretty resp
        else D.logError h $ funcName <> "Failed request: " <> GP.textPretty resp


handleMessage :: (BotClass s, Monad m) => D.Handle s m -> s -> Msg s -> m ()
handleMessage h s m = do
    let
        funcName = "handleMessage: "
        maybeUser = getUser s m
    mReqFunc <- processMessage h s m
    S.withMaybe mReqFunc (return ()) $ \req -> do
        D.logDebug h $ funcName <> "sending some copies of message"
        D.logDebug h $ T.pack $ GP.defaultPretty m
        sendNTimes h s maybeUser req

sendNTimes :: (BotClass s, Monad m) => D.Handle s m -> s -> Maybe (User s) -> m H.HTTPRequest -> m ()
sendNTimes h s maybeUser req = do
    let funcName = "sendNTimes: "
        defaultRepNum = repNum $ D.commonEnv h
    repNraw <- D.findWithDefault h s defaultRepNum maybeUser
    let repN = validateRepNum repNraw
    D.logDebug h $ funcName <> "sending message " <> S.showT repN <> " times."
    reqs <- replicateM repN req

    eithRespList <- mapM (D.sendEcho h) reqs
    let (errs, resps) = partitionEithers eithRespList
    mapM_ (D.logError h . T.pack) errs
    mapM_ (logResponse h s) resps



validateRepNum :: Int -> Int
validateRepNum x
  | x > maxRepNum = maxRepNum
  | x < minRepNum = minRepNum
  | otherwise     = x


