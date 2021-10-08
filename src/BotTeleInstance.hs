{-# LANGUAGE TypeFamilies,
             FlexibleContexts,
             ConstrainedClassMethods,
             GeneralizedNewtypeDeriving,
             OverloadedStrings #-}

module BotTeleInstance where

import BotClassTypes
import BotClass
import BotTypes

import qualified HTTPRequests as H

import TeleTypes
import qualified Data.Aeson as Ae (decode)
import qualified Data.Aeson.Types as AeT (parseEither, parseJSON, toJSON)
import qualified Data.Text.Lazy as TL (Text, pack, fromStrict)
import qualified Data.Text as T (Text, pack)
import Data.Bifunctor (first)

import qualified App.Handle as D

instance BotClassTypes Tele where
    type Conf Tele = TlConfig -- don't care

    type StateC Tele = TlStateConst
    type StateM Tele = TlStateMut
    type Rep Tele = TlReply
    type Upd Tele = TlUpdate
    type Msg Tele = TlMessage
    type Chat Tele = TlChat
    type User Tele = TlUser

    type CallbackQuery Tele = TlCallback


instance BotClass Tele where
    takesJSON _ = True

    --getUpdatesRequest :: (Monad m) => D.Handle s m -> s -> m H.HTTPRequest
    getUpdatesRequest h s = do
        curUpdID <- fmap tlUpdateID $ D.getMutState h s
        return $ req curUpdID
      where tout = timeout $ D.commonEnv h
            url = tlUrl $ D.getConstState h s
            req uid = H.Req H.GET (url <> "getUpdates")
              [("offset", Just $ H.PIntg uid),
               ("timeout", Just $ H.PIntg $ fromIntegral tout)]


    --parseHTTPResponse :: s -> BSL.ByteString -> Either String (Rep s)
    parseHTTPResponse _ resp = do -- Either
        val <- maybe (Left "Couldn't parse HTTP response") Right $ Ae.decode resp
        repl <- AeT.parseEither AeT.parseJSON val
        return repl

--    isSuccess :: s -> Rep s -> Bool
    isSuccess _ = _TR_ok


--    parseUpdatesList :: s -> Rep s -> Either String [Upd s]
    parseUpdatesList d rep = do -- Either
        val <- maybe (Left "Couldn't parse update list") Right $ getResult d rep
        AeT.parseEither AeT.parseJSON val




--    getResult :: s -> Rep s -> Maybe Value
    getResult _ = _TR_result



--    getMsg :: s -> Upd s -> Maybe (Msg s)
    getMsg _ (TlUpdate _ (TEMsg m)) = Just m
    getMsg _ _ = Nothing

   
--
--    getChat :: s -> Msg s -> Maybe (Chat s)
    getChat _ = Just . _TM_chat
--    getUser :: s -> Msg s -> Maybe (User s)
    getUser _ = _TM_from
--    getText :: s -> Msg s -> Maybe T.Text
    getText _ = _TM_text
--    getUserID :: s -> User s -> T.Text
    getUserID _ = T.pack . show . _TUs_id

--    getCallbackQuery :: s -> Upd s -> Maybe (CallbackQuery s)
    getCallbackQuery _ (TlUpdate _ (TECallback cb)) = Just cb
    getCallbackQuery _ _ = Nothing


--    getCallbackUser :: s -> CallbackQuery s -> User s
    getCallbackUser d = _TCB_from
--    getCallbackData :: s -> CallbackQuery s -> Maybe T.Text
    getCallbackData d = _TCB_data
--    getCallbackChat :: s -> CallbackQuery s -> Maybe (Chat s)
    getCallbackChat d c = _TCB_message c >>= return . _TM_chat

    --getUserID :: q -> User q -> T.Text


    --repNumKeyboard :: s -> [Int] -> T.Text -> H.ParamsList
    repNumKeyboard d lst cmd = [("reply_markup", Just $ H.PVal obj)]
      where obj = AeT.toJSON $ repNumKeyboardTele' cmd lst

--    sendTextMsg :: D.Handle s m -> s -> Maybe (Chat s) -> Maybe (User s) -> T.Text
--        -> m (Either String H.HTTPRequest)
    sendTextMsg h s Nothing _ _ = return $ Left "Telegram: no chat supplied, unable to send messages to users"
    sendTextMsg h s (Just c) _ text = do
        let
            url = tlUrl $ D.getConstState h s
        return $ Right $ fmsg url ("sendMessage",
            [("chat_id", Just . H.PIntg . _TC_id $ c),
            ("text", Just $ H.PText text)])
            

--    epilogue :: D.Handle s m -> s -> [Upd s] -> Rep s -> m ()
    epilogue h s [] _ = return ()
    epilogue h s us _ =
        let newUpdateID = (maximum $ map _TU_update_id us) + 1
            f s = s { tlUpdateID = newUpdateID }
        in  D.modifyMutState h s f
--    defaultStateTrans d [] _ m = m
--    defaultStateTrans d us _ m = let newUpdateID = maximum $ map _TU_update_id us
                                 --in  m { tlUpdateID = newUpdateID + 1 }





--    sendMessage' :: (MonadBot (StC q) (StM q) (User q) m) =>
--        q -> StC q -> Msg q -> m (Maybe (State (StM q) H.HTTPRequest))
{-
    sendMessage' d sc m = either
        (\e -> logError (T.pack e) >> return Nothing)
        (return . Just)
            $ sendMessage d sc m
      where sendMessage d sc m =
                let eithMethodParams = sendMessageTele m
                in  fmap (return . fmsg (getUrl d sc)) eithMethodParams
-}

--    processMessage :: (Monad m) => D.Handle s m -> s -> Msg s -> m (Maybe (m H.HTTPRequest))
    processMessage h s m = either
        (\e -> D.logError h (T.pack e) >> return Nothing)
        (return . Just)
            $ sendMessage h s m
      where sendMessage h s m =
                let eithMethodParams = sendMessageTele m
                    url = tlUrl $ D.getConstState h s
                in  fmap (return . fmsg url . first TL.fromStrict) eithMethodParams



{-
    sendTextMsg h s (Just c) _ text stc =
        Right $ return $ fmsg (getUrl d stc) ("sendMessage",
           [("chat_id", Just . H.PIntg . _TC_id $ c),
            ("text", Just $ H.PText text)])
    sendTextMsg d Nothing _ _ _ = Left "Telegram: no chat supplied, unable to send messages to users"
-}

{-
    getUpdatesRequest :: q -> S.Timeout -> StC q -> State (StM q) H.HTTPRequest
    getUpdatesRequest d t sc = do
            curUpdID <- gets tlUpdateID
            return $ req curUpdID
        where req uid = H.Req H.GET (getUrl d sc <> "getUpdates")
                [("offset", Just $ H.PIntg uid),
                 ("timeout", Just $ H.PIntg $ fromIntegral t)]



    defaultConfig _ = defaultTlConfig

    initialize _ (TlConf uid url) = return . Just $ (TLSC url, TLSM uid)

    close _ _ = return ()

    getUpdatesRequest d t sc = do
            curUpdID <- gets tlUpdateID
            return $ req curUpdID
        where req uid = H.Req H.GET (getUrl d sc <> "getUpdates")
                [("offset", Just $ H.PIntg uid),
                 ("timeout", Just $ H.PIntg $ fromIntegral t)]

    parseHTTPResponse _ resp = do -- Either
        val <- maybe (Left "Couldn't parse HTTP response") Right $ Ae.decode resp
        repl <- AeT.parseEither AeT.parseJSON val
        return repl

    parseUpdatesList d rep = do -- Either
        val <- maybe (Left "Couldn't parse update list") Right $ getResult d rep
        AeT.parseEither AeT.parseJSON val

    defaultStateTrans d [] _ m = m
    defaultStateTrans d us _ m = let newUpdateID = maximum $ map _TU_update_id us
                                 in  m { tlUpdateID = newUpdateID + 1 }

    isSuccess _ = _TR_ok
    getResult _ = _TR_result

    getUrl _ = tlUrl

    getMsg _ (TlUpdate _ (TEMsg m)) = Just m
    getMsg _ _ = Nothing

    getText _ = _TM_text

    --getUserID :: q -> User q -> T.Text
    getUserID _ = T.pack . show . _TUs_id

    getChat _ = Just . _TM_chat
    getUser _ = _TM_from

    --getCallBackQuery :: q -> Upd q -> Maybe (CallBackQuery q)
    getCallbackQuery _ (TlUpdate _ (TECallback cb)) = Just cb
    getCallbackQuery _ _ = Nothing

    --getCallBackUser :: q -> CallBackQuery q -> User q
    getCallbackUser d = _TCB_from
    --getCallBackData :: q -> CallBackQuery q -> Maybe T.Text
    getCallbackData d = _TCB_data

    getCallbackChat d c = _TCB_message c >>= return . _TM_chat
    
    sendTextMsg d (Just c) _ text stc =
        Right $ return $ fmsg (getUrl d stc) ("sendMessage",
           [("chat_id", Just . H.PIntg . _TC_id $ c),
            ("text", Just $ H.PText text)])
    sendTextMsg d Nothing _ _ _ = Left "Telegram: no chat supplied, unable to send messages to users"


    --repNumKeyboard :: q -> [Int] -> T.Text -> ParamsList
    repNumKeyboard d lst cmd = [("reply_markup", Just $ H.PVal obj)]
      where obj = AeT.toJSON $ repNumKeyboardTele' cmd lst

--    sendMessage' :: (MonadBot (StC q) (StM q) (User q) m) =>
--        q -> StC q -> Msg q -> m (Maybe (State (StM q) H.HTTPRequest))

    sendMessage' d sc m = either
        (\e -> logError (T.pack e) >> return Nothing)
        (return . Just)
            $ sendMessage d sc m
      where sendMessage d sc m =
                let eithMethodParams = sendMessageTele m
                in  fmap (return . fmsg (getUrl d sc)) eithMethodParams


-}
safeHeadEither [] = Left "Empty list"
safeHeadEither (x:xs) = Right x



{-
    --sendMessage :: q -> StateGeneral -> StC q -> Msg q
    --    -> Either String (State (StM q) HTTPRequest)
    sendMessage d sc m = 
        let eithMethodParams = sendMessageTele m
        in  fmap (return . fmsg (getUrl d sc)) eithMethodParams
-}

