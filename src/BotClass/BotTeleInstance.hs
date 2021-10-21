{-# LANGUAGE
    TypeFamilies
    , RecordWildCards
    #-}

module BotClass.BotTeleInstance where



import Telegram
import BotClass.Class
import BotClass.ClassTypesTeleInstance
import Types
import HTTPRequests as H
import qualified Data.Aeson.Types as AeT (parseEither, parseJSON, toJSON)
import qualified Data.Text.Lazy as TL (fromStrict)
import qualified Data.Text as T (pack)
import Data.Bifunctor (first)
import Data.Foldable (asum)
import qualified App.Handle as D
import qualified GenericPretty as GP
import qualified Stuff as S
import qualified Execute.Logic as E (sendNTimes)
import qualified Control.Monad.Catch as C (throwM)
import qualified Exceptions as Ex

instance BotClassUtility Tele where
--  getResult :: s -> RepSucc s -> Maybe Value
    getResult _ = Just . _TURS_result

--  getMsg :: s -> Upd s -> Maybe (Msg s)
    getMsg _ TlUpdate {..} = case _TU_event of
        TEMsg m -> Just m
        _       -> Nothing

-- getUpdateValue :: s -> Upd s -> Value
    getUpdateValue _ u = _TU_value u

--  getChat :: s -> Msg s -> Maybe (Chat s)
    getChat _ = Just . _TM_chat
--  getUser :: s -> Msg s -> Maybe (User s)
    getUser _ = _TM_from
--  getText :: s -> Msg s -> Maybe T.Text
    getText _ = _TM_text
--  getUserID :: s -> User s -> T.Text
    getUserID _ = T.pack . show . _TUs_id

--  getCallbackQuery :: s -> Upd s -> Maybe (CallbackQuery s)
    getCallbackQuery _ TlUpdate {..} = case _TU_event of
        TECallback cb -> Just cb
        _             -> Nothing



--  getCallbackUser :: s -> CallbackQuery s -> User s
    getCallbackUser _ = _TCB_from
--  getCallbackData :: s -> CallbackQuery s -> Maybe T.Text
    getCallbackData _ = _TCB_data
--  getCallbackChat :: s -> CallbackQuery s -> Maybe (Chat s)
    getCallbackChat _ c = _TCB_message c >>= return . _TM_chat



instance BotClass Tele where
    --takesJSON _ = True
    takesJSON _ = tlTakesJSON

    --getUpdatesRequest :: (Monad m) => D.Handle s m -> s -> m H.HTTPRequest
    getUpdatesRequest h _ = do
        let
            tout = timeout $ D.commonEnv h
            url = tlUrl $ D.getConstState h
            req uid = H.Req H.GET (url <> "getUpdates")
              [unit "offset" uid,
               unit "timeout" tout]
        curUpdID <- getUpdateID (D.specH h)
        return $ req curUpdID

--  isSuccess :: s -> Rep s -> Bool
    isSuccess _ = _TR_ok

--  handleFailedUpdatesRequest :: (C.MonadThrow m) => D.Handle s m -> RepErr s -> m ()
    handleFailedUpdatesRequest h err = do
        let funcName = "tl_handleFailedUpdatesRequest: "
        D.logError h $ funcName <> "got telegram error"
        D.logError h $ GP.defaultPrettyT err
        D.logError h $ funcName <> "unable to handle any telegram errors"
        C.throwM Ex.UnableToHandleError
--  normal work is now impossible since the bot can't handle any errors

--  parseUpdatesValueList :: s -> RepSucc s -> Either String [Value]
    parseUpdatesValueList s rep = do
        res <- maybe (Left "Couldn't parse update list") Right $ getResult s rep
        AeT.parseEither AeT.parseJSON res

--  parseUpdate :: s -> Value -> Either String (Upd s)
    parseUpdate _ = AeT.parseEither AeT.parseJSON


    --repNumKeyboard :: s -> [Int] -> T.Text -> H.ParamsList
    repNumKeyboard _ lst cmd = [unit "reply_markup" obj]
      where obj = AeT.toJSON $ repNumKeyboardTele' cmd lst

--  sendTextMsg :: D.Handle s m -> s -> Maybe (Chat s) -> Maybe (User s) -> T.Text
--      -> m (Either String H.HTTPRequest)
    sendTextMsg _ _ Nothing _ _ = return $ Left "Telegram: no chat supplied, unable to send messages to users"
    sendTextMsg h _ (Just c) _ text =
        let
            url = tlUrl $ D.getConstState h
        in  return $ Right $ buildHTTP url ("sendMessage",
            [unit "chat_id" $ _TC_id c,
             unit "text" text])
            

--  epilogue :: D.Handle s m -> s -> [Upd s] -> Rep s -> m ()
    epilogue _ _ [] _ = return ()
    epilogue h _ us _ = do
        let
            funcName = "tl_epilogue: "
            newUpdateID = (maximum $ map _TU_update_id us) + 1
        putUpdateID (D.specH h) newUpdateID
        mediaGroups <- getMediaGroups (D.specH h)
        D.logDebug h $ funcName <> "ready to process some media groups, if any"
        D.logDebug h $ GP.defaultPrettyT mediaGroups
        mapM_ (sendMediaGroup h) mediaGroups
        purgeMediaGroups (D.specH h)

--  processMessage :: (Monad m) => D.Handle s m -> s -> Msg s -> m (Maybe (m H.HTTPRequest))
    processMessage = processMessage1


processMessage1 :: (Monad m) => D.Handle Tele m -> Tele -> TlMessage -> m (Maybe (m H.HTTPRequest))
processMessage1 h _ m =
  if isMediaGroup m
  then processMediaGroup h m >> return Nothing
  else either
    (\e -> do
        D.logError h $ funcName <> T.pack e
        return Nothing)
    (return . Just)
        $ sendMessage
  where funcName = "tl_processMessage: "
        sendMessage =
            let eithMethodParams = sendMessageTele m
                url = tlUrl $ D.getConstState h
            in  return . buildHTTP url . first TL.fromStrict <$> eithMethodParams


processMediaGroup :: (Monad m) => D.Handle Tele m -> TlMessage -> m ()
processMediaGroup h m = let
    funcName = "processMediaGroup: "
    chat = _TM_chat m
    mUser = _TM_from m
    mMediaGroupID = _TM_media_group_id m
    mMediaGroupIdent = TlMediaGroupIdentifier chat mUser <$> mMediaGroupID
    mMediaGroupUnit = maybeMediaGroupUnit m
    mAction = asum [ insertMediaGroupUnit (D.specH h) <$> mMediaGroupIdent <*> mMediaGroupUnit ]
    processFail = do
        D.logWarning h $ funcName <> "failed to process media group. Message:"
        D.logWarning h $ GP.textPretty m
    in S.withMaybe mAction (processFail) $ \action -> do
            D.logDebug h $ funcName <> "processing media group"
            action


sendMediaGroup :: (Monad m) => D.Handle Tele m -> TlMediaGroupPair -> m ()
sendMediaGroup h (TlMediaGroupPair ident items) = do
    let chat = _TMGI_chat ident
        mUser = _TMGI_user ident
        items' = reverse items
        method = "sendMediaGroup"
        url = tlUrl $ D.getConstState h
        mCaption = S.safeHead items >>= photoVideoCaption
        pars = [unit "chat_id" $ _TC_id chat
                , unit "media" $ AeT.toJSON items'
                , mUnit "caption" mCaption]
        req = buildHTTP url (method, pars)
    E.sendNTimes h Tele mUser (return req)

