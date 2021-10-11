{-# LANGUAGE TypeFamilies
             , FlexibleContexts
             , ConstrainedClassMethods
             , GeneralizedNewtypeDeriving
             , RecordWildCards
    #-}

module BotClass.BotTeleInstance where

import BotClass.Class
import BotClass.ClassTypes
import BotClass.ClassTypesTeleInstance
import Telegram.Types
import Telegram.MediaGroup
import Types

import HTTPRequests as H

import qualified Data.Aeson as Ae (decode)
import qualified Data.Aeson.Types as AeT (parseEither, parseJSON, toJSON)
import qualified Data.Text.Lazy as TL (Text, pack, fromStrict)
import qualified Data.Text as T (Text, pack)
import Data.Bifunctor (first)
import Data.Foldable (asum)

import qualified App.Handle as D
import qualified GenericPretty as GP
import qualified Stuff as S
import qualified Execute as E

instance BotClassUtility Tele where
--    getResult :: s -> RepSucc s -> Maybe Value
    getResult _ = Just . _TURS_result

--    getMsg :: s -> Upd s -> Maybe (Msg s)
    getMsg _ TlUpdate {..} = case _TU_event of
        TEMsg m -> Just m
        _       -> Nothing

--   getUpdateValue :: s -> Upd s -> Value
    getUpdateValue _ u = _TU_value u

--    getChat :: s -> Msg s -> Maybe (Chat s)
    getChat _ = Just . _TM_chat
--    getUser :: s -> Msg s -> Maybe (User s)
    getUser _ = _TM_from
--    getText :: s -> Msg s -> Maybe T.Text
    getText _ = _TM_text
--    getUserID :: s -> User s -> T.Text
    getUserID _ = T.pack . show . _TUs_id

--    getCallbackQuery :: s -> Upd s -> Maybe (CallbackQuery s)
    getCallbackQuery _ TlUpdate {..} = case _TU_event of
        TECallback cb -> Just cb
        _             -> Nothing



--    getCallbackUser :: s -> CallbackQuery s -> User s
    getCallbackUser d = _TCB_from
--    getCallbackData :: s -> CallbackQuery s -> Maybe T.Text
    getCallbackData d = _TCB_data
--    getCallbackChat :: s -> CallbackQuery s -> Maybe (Chat s)
    getCallbackChat d c = _TCB_message c >>= return . _TM_chat



instance BotClass Tele where
    takesJSON _ = True

    --getUpdatesRequest :: (Monad m) => D.Handle s m -> s -> m H.HTTPRequest
    getUpdatesRequest h s = do
        let
            tout = timeout $ D.commonEnv h
            url = tlUrl $ D.getConstState h
            req uid = H.Req H.GET (url <> "getUpdates")
              [unit "offset" uid,
               unit "timeout" tout]
        curUpdID <- getUpdateID (D.specH h)
        return $ req curUpdID

--    parseHTTPResponse :: s -> BSL.ByteString -> Either String (Rep s)
    parseHTTPResponse _ resp = do -- Either
        val <- maybe (Left "Couldn't parse HTTP response") Right $ Ae.decode resp
        repl <- AeT.parseEither AeT.parseJSON val
        return repl

--    isSuccess :: s -> Rep s -> Bool
    isSuccess _ = _TR_ok

--    parseUpdatesResponse :: s -> BSL.ByteString -> Either String (UpdateResponse (RepSucc s) (RepErr s))
    parseUpdatesResponse _ resp = do -- Either
        val <- maybe (Left "Couldn't parse updates response into aeson Value") Right $ Ae.decode resp
        repl <- parseUpdatesResponse1 val
        return repl

--    handleFailedUpdatesRequest :: (C.MonadThrow m) => D.Handle s m -> RepErr s -> m ()
    handleFailedUpdatesRequest = undefined

--    parseUpdatesValueList :: s -> RepSucc s -> Either String [Value]
    parseUpdatesValueList s rep = do
        res <- maybe (Left "Couldn't parse update list") Right $ getResult s rep
        AeT.parseEither AeT.parseJSON res
        --Left "debug error"

--    parseUpdate :: s -> Value -> Either String (Upd s)
    parseUpdate s = AeT.parseEither AeT.parseJSON


    --repNumKeyboard :: s -> [Int] -> T.Text -> H.ParamsList
    repNumKeyboard d lst cmd = [unit "reply_markup" obj]
      where obj = AeT.toJSON $ repNumKeyboardTele' cmd lst

--    sendTextMsg :: D.Handle s m -> s -> Maybe (Chat s) -> Maybe (User s) -> T.Text
--        -> m (Either String H.HTTPRequest)
    sendTextMsg h s Nothing _ _ = return $ Left "Telegram: no chat supplied, unable to send messages to users"
    sendTextMsg h s (Just c) _ text =
        let
            url = tlUrl $ D.getConstState h
        in  return $ Right $ fmsg url ("sendMessage",
            [unit "chat_id" $ _TC_id c,
             unit "text" text])
            

--    epilogue :: D.Handle s m -> s -> [Upd s] -> Rep s -> m ()
    epilogue h s [] _ = return ()
    epilogue h s us _ = do
        let
            funcName = "tl_epilogue: "
            newUpdateID = (maximum $ map _TU_update_id us) + 1
        putUpdateID (D.specH h) newUpdateID
        mediaGroups <- getMediaGroups (D.specH h)
        D.logDebug h $ funcName <> "ready to process some media groups, if any"
        D.logDebug h $ GP.defaultPrettyT mediaGroups
        mapM_ (sendMediaGroup h) mediaGroups
        purgeMediaGroups (D.specH h)

--    processMessage :: (Monad m) => D.Handle s m -> s -> Msg s -> m (Maybe (m H.HTTPRequest))
    processMessage = processMessage1


processMessage1 h s m =
  if isMediaGroup m
  then processMediaGroup h m >> return Nothing
  else either
    (\e -> do
        D.logError h $ funcName <> T.pack e
        return Nothing)
    (return . Just)
        $ sendMessage h s m
  where funcName = "tl_processMessage: "
        sendMessage h s m =
            let eithMethodParams = sendMessageTele m
                url = tlUrl $ D.getConstState h
                notMediaGroup = fmap (return . fmsg url . first TL.fromStrict) eithMethodParams
            in  notMediaGroup


processMediaGroup :: (Monad m) => D.Handle Tele m -> TlMessage -> m ()
processMediaGroup h m = let
    funcName = "processMediaGroup: "
    chat = _TM_chat m
    mUser = _TM_from m
    mMediaGroupID = _TM_media_group_id m
    mPhoto = _TM_photo m >>= S.safeHead :: Maybe TlPhotoSize
    mMediaGroupIdent = TlMediaGroupIdentifier chat mUser <$> mMediaGroupID
    mAction = asum [ insertMediaGroupPhoto (D.specH h) <$> mMediaGroupIdent <*> mPhoto ]
    in S.withMaybe mAction (return ()) $
         \a -> do
            D.logDebug h $ funcName <> "processing media group"
            a


sendMediaGroup :: (Monad m) => D.Handle Tele m -> TlMediaGroupPair -> m ()
sendMediaGroup h (TlMediaGroupPair ident items) = do
    let chat = _TMGI_chat ident
        mUser = _TMGI_user ident
        items' = map func items
        sc = D.getConstState h
        method = "sendMediaGroup"
        url = tlUrl $ D.getConstState h -- нахрена тут этот параметр? убрать!
        pars = [unit "chat_id" $ _TC_id chat,
                unit "media" $ AeT.toJSON items']
        req = fmsg url (method, pars)
    --E.sendFixedInfo h Tele req
    E.sendNTimes h Tele mUser (return req)

func :: TlPhotoSize -> TlInputMediaPhoto
func photo =
    TlInputMediaPhoto {
        _TIMP_media = _TPS_file_id photo
    }


