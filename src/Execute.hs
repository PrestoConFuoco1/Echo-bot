module Execute where

import qualified App.Handle as D
import BotClass
import BotClassTypes
import BotTypes
import qualified Data.Text as T
import qualified GenericPretty as GP
import qualified Stuff as S
import Data.Foldable (asum)
import qualified HTTPRequests as H
import Text.Read (readMaybe)
import Control.Monad (replicateM)
import Data.Either (partitionEithers)

mainLoop :: (BotClass s, Monad m) => D.Handle s m -> s -> m ()
mainLoop h s = do
    --let env = commonEnv h
    let respParseFail =
            D.logError h . ("failed to parse server reply: " <>) . T.pack
        updLstParseFail =
            D.logError h . ("failed to parse update list: " <>) . T.pack

    request <- getUpdatesRequest h s
    eithRespStr <- D.sendRequest h (takesJSON s) request -- Either String a
    let eithResp = eithRespStr >>= parseHTTPResponse s
    -- ($ eithResp) $ either respParseFail $ \resp -> do
    S.withEither eithResp respParseFail $ \resp -> do
        D.logDebug h $ "Got and successfully parsed server reply:"
        D.logDebug h $ T.pack $ GP.defaultPretty resp
        if not $ isSuccess s resp
        then undefined
        else do
            let eithUpdList = parseUpdatesList s resp
            --($ eithUpdList) $ either updLstParseFail $ \updLst -> do
            S.withEither eithUpdList updLstParseFail $ \updLst -> do
                mapM_ (handleUpdate h s) updLst
                epilogue h s updLst resp

handleUpdate :: (BotClass s, Monad m) =>
    D.Handle s m -> s -> Upd s -> m ()
handleUpdate h s u = do
  case defineUpdateType h s u of
    ECommand cmd mChat mUser -> handleCommand h s cmd mChat mUser
    EMessage m -> handleMessage h s m
    ECallback callback -> handleCallback h s callback
    EError err -> D.logError h err

--type EventT s = Event (Chat s) (User s) (Msg s) (CallbackQuery s)

defineUpdateType :: (BotClass s) => D.Handle s m -> s -> Upd s
    -> Event (Chat s) (User s) (Msg s) (CallbackQuery s)
--    -> EventT s
defineUpdateType h s u =
    let mMsg = getMsg s u
        mText = mMsg >>= getText s
        mUser = mMsg >>= getUser s
        mChat = mMsg >>= getChat s
        mCmd = mText >>= S.safeHead . T.words
            >>= (getCmd $ D.commonEnv h)
        mCallback = getCallbackQuery s u
    in  maybe (EError "Unknown type. Wtf?") id $
          asum [
                ECallback <$> mCallback,
                ECommand <$> mCmd <*> Just mChat <*> Just mUser,
                EMessage <$> mMsg]

getCmd :: EnvironmentCommon -> T.Text -> Maybe Command
getCmd e str
    | str == helpCommand e = Just Help
    | str == setRepNumCommand e = Just SetRepNum
    | otherwise = Nothing

handleMessage :: (BotClass s, Monad m) => D.Handle s m -> s -> Msg s -> m ()
handleMessage h s m = do
    let
        maybeUser = getUser s m
        defaultRepNum = repNum $ D.commonEnv h
    mReqFunc <- processMessage h s m
    ($ mReqFunc) $ maybe (return ()) $ \req -> do
        repN <- D.findWithDefault h s defaultRepNum maybeUser
        D.logDebug h $ "Sending message " <> S.showT repN <> " times."
        D.logDebug h $ T.pack $ GP.defaultPretty m
        reqs <- replicateM repN req
        eithRespStrList <- mapM (D.sendRequest h $ takesJSON s) reqs
        let (errs, resps) = partitionEithers $
                map (>>= parseHTTPResponse s) eithRespStrList
        mapM_ (D.logError h . T.pack) errs


handleCallback :: (BotClass s, Monad m) =>
    D.Handle s m -> s -> CallbackQuery s -> m ()
handleCallback h s callback =
    case defineCallbackType h s callback of
        CSetRepNum user mChat n -> handleSetRepNum h s user mChat n
        CError err -> D.logError h $ T.pack err

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
                    maybe (Left "Expected number after \"set\" command.")
--- !!! move "set" to config?
                    Right (readMaybe $ T.unpack num :: Maybe Int)
                _ -> Left "Unknown callback query"
            return repNumDat
    in  either CError id $
          asum [ CSetRepNum user mChat <$> eithSetN ]



handleSetRepNum :: (BotClass s, Monad m) =>
    D.Handle s m -> s -> User s -> Maybe (Chat s) -> Int -> m ()
handleSetRepNum h s user mChat repnum = do
    let text = "Now every your message will be repeated "
            <> S.showT repnum <> " times."
        afterLog = "User with ID = " <> getUserID s user
            <> " set " <> S.showT repnum <> " repeats."
        sendFail x = D.logError h $ "Failed to send message: " <> T.pack x
    D.insertUser h s user repnum
    eithReqFunc <- sendTextMsg h s mChat (Just user) text
    D.logInfo h afterLog
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
    eithReqFunc <- sendTextMsg h s mChat mUser (helpMsg $ D.commonEnv h)
    D.logDebug h $ "Sending HTTP request to send help message"
    ($ eithReqFunc) $ either (D.logError h . T.pack) $ \req -> do
        sendFixedInfo h s req


minRepNum, maxRepNum :: Int
minRepNum = 1
maxRepNum = 5 

sendRepNumButtons :: (BotClass s, Monad m) =>
    D.Handle s m -> s -> Maybe (Chat s) -> Maybe (User s) -> m ()
sendRepNumButtons h s mChat mUser = do
    eithReqFunc <- sendTextMsg h s mChat mUser (repQuestion $ D.commonEnv h)
    let eithReqFuncKeyboard = fmap (H.addParams inlKeyboardPars) eithReqFunc
        inlKeyboardPars = repNumKeyboard s [minRepNum..maxRepNum] "set"
    D.logDebug h $ "Sending HTTP request to send repeat number buttons"
    ($ eithReqFuncKeyboard) $ either (D.logError h . T.pack) $ \req -> do
        sendFixedInfo h s req

sendFixedInfo :: (BotClass s, Monad m) =>
    D.Handle s m -> s -> H.HTTPRequest -> m ()
sendFixedInfo h s request = do
    eithRespStr <- D.sendRequest h (takesJSON s) request
    let eithResp = eithRespStr >>= parseHTTPResponse s >>= f
        f resp' = if isSuccess s resp'
                  --then Right $ "Ok: " <> S.showT resp'
                  then Right $ "Ok: " <> (T.pack $ GP.defaultPretty resp')
                  else Left $ "Failed request: " ++ show resp'
    either (D.logError h . T.pack) (D.logInfo h) eithResp


