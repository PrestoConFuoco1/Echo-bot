{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Execute
    ( execute
    ) where

import qualified App.BotHandler as BotH
import BotTypesClass.ClassTypes (BotClassTypes(..))
import Control.Monad (when)
import qualified Control.Monad.Catch as C
import qualified Data.Aeson as Ae
import Data.Either (partitionEithers)
import qualified Data.Text as T
import qualified Exceptions as Ex
import Execute.BotClass (BotClass(..))
import Execute.Logic (handleUpdate)
import qualified GenericPretty as GP
import qualified Stuff as S

execute :: (BotClass s, C.MonadThrow m) => BotH.BotHandler s m -> m ()
execute h = do
    let funcName = "execute: "
        respParseFail =
            BotH.logError h .
            ("failed to parse server reply: " <>) .
            (funcName <>) . T.pack
    request <- getUpdatesRequest h
    eithResp <- BotH.getUpdates h request
    S.withEither eithResp respParseFail $ \resp' -> do
        BotH.logDebug h $
            funcName <> "got and successfully parsed server reply:"
        BotH.logDebug h $ GP.textPretty resp'
        case resp' of
            Left updErr -> handleFailedUpdatesRequest h updErr
            Right resp -> handleUpdatesSuccess h resp

updateListParseFail ::
       (C.MonadThrow m) => BotH.BotHandler s m -> String -> m a
updateListParseFail h x = do
    let funcName = "updateListParseFail: "
    BotH.logError h .
        ("failed to parse update list: " <>) . (funcName <>) . T.pack $
        x
    C.throwM Ex.FailedToParseUpdatesListFromResult

-- normal work is impossible if we are not able to parse update list
logUpdatesErrors ::
       (BotClass s, C.MonadThrow m)
    => BotH.BotHandler s m
    -> [(Ae.Value, String)]
    -> [Upd s]
    -> m ()
logUpdatesErrors h errs upds = do
    let funcName = "logUpdatesErrors: "
    BotH.logError h $ funcName <> "failed to parse some updates"
    mapM_ (singleUpdateParseFail h) errs
    when (null upds) $ do
        BotH.logError h $ funcName <> "failed to parse all updates"
        C.throwM Ex.ParsedNoUpdates

-- normal work is impossible if we are not able to parse any updates
-- we will try to parse them again and again resulting an infinite worthless loop
handleUpdatesSuccess ::
       forall s m. (BotClass s, C.MonadThrow m)
    => BotH.BotHandler s m
    -> RepSucc s
    -> m ()
handleUpdatesSuccess h resp = do
    let eithUpdValueList = parseUpdatesValueList @s resp
    S.withEither eithUpdValueList (updateListParseFail h) $ \updValues -> do
        let parsedUpdates =
                map (attachErrorReason $ parseUpdate @s) updValues
            (errs, upds) = partitionEithers parsedUpdates
            errLength = length errs
        when (errLength > 0) $ logUpdatesErrors @s h errs upds
        mapM_ (handleUpdate @s h) upds
        epilogue @s h upds resp

singleUpdateParseFail ::
       forall s m. (BotClass s, Monad m)
    => BotH.BotHandler s m
    -> (Ae.Value, String)
    -> m ()
singleUpdateParseFail h (u, e) = do
    let funcName = "singleUpdateParseFail: "
    BotH.logError h $
        funcName <>
        "Failed to parse update: <" <> T.pack e <> ">, update is:"
    BotH.logError h $ GP.textPretty u

attachErrorReason :: (a -> Either b c) -> a -> Either (a, b) c
attachErrorReason f x =
    case f x of
        Right y -> Right y
        Left e -> Left (x, e)
