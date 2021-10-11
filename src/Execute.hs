module Execute where

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
import Control.Monad (replicateM, when)
import Data.Either (partitionEithers)
import qualified Control.Monad.Catch as C
import qualified Exceptions as Ex

mainLoop :: (BotClass s, C.MonadThrow m) => D.Handle s m -> s -> m ()
mainLoop h s = do
    --let env = commonEnv h
    let funcName = "mainLoop: "
        respParseFail =
            D.logError h . ("failed to parse server reply: " <>) . (funcName <>) . T.pack
        updLstParseFail x = do
            D.logError h . ("failed to parse update list: " <>) . (funcName <>) . T.pack $ x
            C.throwM Ex.FailedToParseUpdatesListFromResult
            

    request <- getUpdatesRequest h s
    eithRespStr <- D.sendRequest h (takesJSON s) request -- Either String a
    let eithResp = eithRespStr >>= parseHTTPResponse s
    -- ($ eithResp) $ either respParseFail $ \resp -> do
    S.withEither eithResp respParseFail $ \resp -> do
        D.logDebug h $ funcName <> "got and successfully parsed server reply:"
        D.logDebug h $ T.pack $ GP.defaultPretty resp
        if not $ isSuccess s resp
        then undefined
        else do
            let eithUpdValueList = parseUpdatesValueList s resp
            S.withEither eithUpdValueList updLstParseFail
                $ \updValues -> do
                let parsedUpdates = map (func $ parseUpdate s) updValues
                    (errs, upds) = partitionEithers parsedUpdates
                    errLength = length errs
                when (errLength > 0) $ do
                    let g (u, e) = do
                        D.logError h $ funcName <> "Failed to parse update: <" <> T.pack e <> ">, update is:"
                        D.logError h $ GP.defaultPrettyT u
                    D.logError h $ funcName <> "failed to parse some updates"
                    mapM_ g errs
                    --when (null upds) $ error "failed to parse all updates"
                    when (null upds) $ C.throwM Ex.ParsedNoUpdates
                mapM_ (handleUpdate h s) upds
                epilogue h s upds resp

func :: (a -> Either b c) -> a -> Either (a, b) c
func f x = case f x of
    Right y -> Right y
    Left  e -> Left (x, e)

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
    case defineCallbackType h s callback of
        CSetRepNum user mChat n -> handleSetRepNum h s user mChat n
        CError err -> D.logError h $ "handleCallback: " <> T.pack err

defineCallbackType :: (BotClass s) =>
    D.Handle s m -> s -> CallbackQuery s -> CallbQuery (User s) (Chat s)
defineCallbackType h s callback =
    let user  = getCallbackUser s callback
        mData = getCallbackData s callback
        mChat = getCallbackChat s callback
        eithSetN = do
            dat <- maybe
                (Left "No callback data found, unable to respond.") Right mData
            repNumDat <- case T.words dat of
                ("set" : num : _) ->
                    -- maybe move "set" to config?
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
    either sendFail (sendFixedInfo h s) eithReqFunc

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
        (sendFixedInfo h s)


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
        (sendFixedInfo h s)

sendFixedInfo :: (BotClass s, Monad m) =>
    D.Handle s m -> s -> H.HTTPRequest -> m ()
sendFixedInfo h s request = do
    let funcName = "sendFixedInfo: "
    eithRespStr <- D.sendRequest h (takesJSON s) request
    let eithResp = eithRespStr >>= parseHTTPResponse s >>= f
        f resp' = if isSuccess s resp'
                  --then Right $ "Ok: " <> S.showT resp'
                  then Right $ "Ok: " <> (T.pack $ GP.defaultPretty resp')
                  else Left $ "Failed request: " ++ show resp'
    S.withEither eithResp
        (D.logError h . (funcName <>) . T.pack)
        (D.logInfo h . (funcName <>))


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
    eithRespStrList <- mapM (D.sendRequest h $ takesJSON s) reqs
    let (errs, resps) = partitionEithers $
            map (>>= parseHTTPResponse s) eithRespStrList
    mapM_ (D.logError h . T.pack) errs



validateRepNum :: Int -> Int
validateRepNum x
  | x > maxRepNum = maxRepNum
  | x < minRepNum = minRepNum
  | otherwise     = x


