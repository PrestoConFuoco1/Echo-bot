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
import qualified Data.Aeson as Ae
import Execute.Logic

execute :: (BotClass s, C.MonadThrow m) => D.Handle s m -> s -> m ()
execute h s = do
    --let env = commonEnv h
    let funcName = "mainLoop: "
        respParseFail =
            D.logError h . ("failed to parse server reply: " <>) . (funcName <>) . T.pack

    request <- getUpdatesRequest h s
{-
    eithRespStr <- D.sendRequest h (takesJSON s) request -- Either String a
    let eithResp = eithRespStr >>= parseUpdatesResponse s
-} 
    eithResp <- D.getUpdates h request
    S.withEither eithResp respParseFail $ \resp' -> do
        D.logDebug h $ funcName <> "got and successfully parsed server reply:"
        D.logDebug h $ T.pack $ GP.defaultPretty resp'
        case resp' of
          UpdateError updErr  -> handleFailedUpdatesRequest h updErr
          UpdateResponse resp -> handleUpdatesSuccess h s resp

updLstParseFail :: (C.MonadThrow m) => D.Handle s m -> String -> m a
updLstParseFail h x = do
    let funcName = "updateListParseFail: "
    D.logError h . ("failed to parse update list: " <>) . (funcName <>) . T.pack $ x
    C.throwM Ex.FailedToParseUpdatesListFromResult


logUpdatesErrors :: (BotClass s, C.MonadThrow m) => D.Handle s m -> [(Ae.Value, String)] -> [Upd s] -> m ()
logUpdatesErrors h errs upds = do
    let funcName = "logUpdatesErrors: "
    D.logError h $ funcName <> "failed to parse some updates"
    mapM_ (singleUpdateParseFail h) errs
    when (null upds) $ do
        D.logError h $ funcName <> "failed to parse all updates"
        C.throwM Ex.ParsedNoUpdates
 

handleUpdatesSuccess :: (BotClass s, C.MonadThrow m) => D.Handle s m -> s -> RepSucc s -> m ()
handleUpdatesSuccess h s resp = do
    let funcName = "handleUpdatesSuccess: "
        eithUpdValueList = parseUpdatesValueList s resp
    S.withEither eithUpdValueList (updLstParseFail h) $ \updValues -> do
        let parsedUpdates = map (func $ parseUpdate s) updValues
            (errs, upds) = partitionEithers parsedUpdates
            errLength = length errs
        when (errLength > 0) $ logUpdatesErrors h errs upds
        mapM_ (handleUpdate h s) upds
        epilogue h s upds resp


singleUpdateParseFail :: (BotClass s, Monad m) => D.Handle s m -> (Ae.Value, String) -> m ()
singleUpdateParseFail h (u, e) = do
    let funcName = "singleUpdateParseFail: "
    D.logError h $ funcName <> "Failed to parse update: <" <> T.pack e <> ">, update is:"
    D.logError h $ GP.defaultPrettyT u
 

{-
-}
func :: (a -> Either b c) -> a -> Either (a, b) c
func f x = case f x of
    Right y -> Right y
    Left  e -> Left (x, e)

