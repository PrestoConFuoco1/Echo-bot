module Execute where

import qualified App.Handle as D
import BotClass
import BotTypes
import qualified Data.Text as T
import qualified GenericPretty as GP
import qualified Stuff as S
import Data.Foldable (asum)
import qualified HTTPRequests as H

mainLoop :: (BotClass s, Monad m) => D.Handle m -> s -> m ()
mainLoop h s = do
    --let env = commonEnv h
    let respParseFail =
            D.logError h . ("failed to parse server reply: " <>) . T.pack
        updLstParseFail =
            D.logError h . ("failed to parse update list: " <>) . T.pack

    request <- getUpdatesRequest h s
    eithRespStr <- D.sendRequest h (takesJSON s) request -- Either String a
    let eithResp = eithRespStr >>= parseHTTPResponse s
    ($ eithResp) $ either respParseFail $ \resp -> do
        D.logDebug h $ "Got and successfully parsed server reply:"
        D.logDebug h $ T.pack $ GP.defaultPretty resp
        if not $ isSuccess s resp
        then undefined
        else do
            let eithUpdList = parseUpdatesList s resp
            ($ eithUpdList) $ either updLstParseFail $ \updLst -> do
                mapM_ (handleUpdate h s) updLst
                defaultStateTrans h s updLst resp

handleUpdate :: (BotClass s, Monad m) =>
--        , Ord (User s),
--        Show (Upd s), Show (User s), Show (Rep s), Show (Msg s),
--        MonadBot (StC s) (StM s) (User s) m,
--        GP.PrettyShow (Msg s),
--        GP.PrettyShow (Rep s)) =>
    D.Handle m -> s -> Upd s -> m ()
handleUpdate h s u = do
  case defineUpdateType h s u of
    ECommand cmd mChat mUser -> handleCommand h s cmd mChat mUser
    EMessage m -> handleMessage h s m
    ECallback callback -> handleCallback h s callback
    EError err -> D.logError h err

--type EventT s = Event (Chat s) (User s) (Msg s) (CallbackQuery s)

defineUpdateType :: (BotClass s) => D.Handle m -> s -> Upd s
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


handleMessage = undefined

handleCallback :: (BotClass s, Monad m) =>
--        , Ord (User s), Show (Rep s),
--        MonadBot (StC s) (StM s) (User s) m,
--        GP.PrettyShow (Rep s)) =>
    D.Handle m -> s -> CallbackQuery s -> m ()
handleCallback h s callback =
    case defineCallbackType h s callback of
        CSetRepNum user mChat n -> handleSetRepNum h s user mChat n
        CError err -> D.logError h $ T.pack err

defineCallbackType = undefined

handleSetRepNum :: (BotClass s, Monad m) =>
--        Ord (User s), Show (Rep s),
--        MonadBot (StC s) (StM s) (User s) m,
--        GP.PrettyShow (Rep s)) =>
    D.Handle m -> s -> User s -> Maybe (Chat s) -> Int -> m ()
handleSetRepNum h s user mChat repnum = do
    let text = "Now every your message will be repeated "
            <> S.showT repnum <> " times."
        afterLog = "User with ID = " <> getUserID s user
            <> " set " <> S.showT repnum <> " repeats."
        sendFail x = D.logError h $ "Failed to send message: " <> T.pack x
    error "insertUser not implemented"
    --D.insertUser h user n
    eithReqFunc <- sendTextMsg h s mChat (Just user) text
    D.logInfo h afterLog
    either sendFail (sendFixedInfo h s) eithReqFunc
    
 
{-
handleSetRepNum d user mChat n = do
    (StTotConst _ sc) <- getConstState'
    let eithReqFunc = fmap liftStateSpec
            $ sendTextMsg d mChat (Just user) text sc
        text = "Now every your message will be repeated "
            <> S.showT n <> " times."
        afterLog = "User with ID = " <> getUserID d user
            <> " set " <> S.showT n <> " repeats."
        sendFail x = logError $ "Failed to send message: " <> T.pack x

    withMutState' $ modifyStateMap (M.insert user n)
    logInfo afterLog
    ($ eithReqFunc) $ either sendFail $ sendFixedInfo d "send repnum message."
-}


handleCommand :: (BotClass s, Monad m) =>
--        , Show (Rep s),
--        MonadBot (StC s) (StM s) (User s) m,
--        GP.PrettyShow (Rep s)) =>
    D.Handle m -> s -> Command -> Maybe (Chat s) -> Maybe (User s) -> m ()
handleCommand h s cmd mChat mUser =
  case cmd of 
    Help -> sendHelp h s mChat mUser
    SetRepNum -> sendRepNumButtons h s mChat mUser 

sendHelp :: (BotClass s, Monad m) =>
--        , Show (Rep s), MonadBot (StC s) (StM s) (User s) m,
--        GP.PrettyShow (Rep s)) =>
    D.Handle m -> s -> Maybe (Chat s) -> Maybe (User s) -> m ()
sendHelp h s mChat mUser = do
    eithReqFunc <- sendTextMsg h s mChat mUser (helpMsg $ D.commonEnv h)
    D.logDebug h $ "Sending HTTP request to send help message"
    ($ eithReqFunc) $ either (D.logError h . T.pack) $ \req -> do
        sendFixedInfo h s req


minRepNum, maxRepNum :: Int
minRepNum = 1
maxRepNum = 5 

sendRepNumButtons :: (BotClass s, Monad m) =>
--        Show (Rep s),
--        MonadBot (StC s) (StM s) (User s) m,
--        GP.PrettyShow (Rep s)) =>
    D.Handle m -> s -> Maybe (Chat s) -> Maybe (User s) -> m ()
sendRepNumButtons h s mChat mUser = do
    eithReqFunc <- sendTextMsg h s mChat mUser (repQuestion $ D.commonEnv h)
    let eithReqFuncKeyboard = fmap (H.addParams inlKeyboardPars) eithReqFunc
        inlKeyboardPars = repNumKeyboard s [minRepNum..maxRepNum] "set"
    D.logDebug h $ "Sending HTTP request to send repeat number buttons"
    ($ eithReqFuncKeyboard) $ either (D.logError h . T.pack) $ \req -> do
        sendFixedInfo h s req

sendFixedInfo :: (BotClass s, Monad m) =>
--        , Show (Rep s),
--        MonadBot (StC s) (StM s) (User s) m,
--        GP.PrettyShow (Rep s)) =>
    D.Handle m -> s -> H.HTTPRequest -> m ()
sendFixedInfo h s request = do
    eithRespStr <- D.sendRequest h (takesJSON s) request
    let eithResp = eithRespStr >>= parseHTTPResponse s >>= f
        f resp' = if isSuccess s resp'
                  --then Right $ "Ok: " <> S.showT resp'
                  then Right $ "Ok: " <> (T.pack $ GP.defaultPretty resp')
                  else Left $ "Failed request: " ++ show resp'
    either (D.logError h . T.pack) (D.logInfo h) eithResp


