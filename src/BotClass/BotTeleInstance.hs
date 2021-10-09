{-# LANGUAGE TypeFamilies,
             FlexibleContexts,
             ConstrainedClassMethods,
             GeneralizedNewtypeDeriving,
             OverloadedStrings #-}

module BotClass.BotTeleInstance where

import BotClass.Class
import BotClass.ClassTypes
import BotClass.ClassTypesTeleInstance
import Telegram.Types
import Types

import qualified HTTPRequests as H

import qualified Data.Aeson as Ae (decode)
import qualified Data.Aeson.Types as AeT (parseEither, parseJSON, toJSON)
import qualified Data.Text.Lazy as TL (Text, pack, fromStrict)
import qualified Data.Text as T (Text, pack)
import Data.Bifunctor (first)

import qualified App.Handle as D


instance BotClassUtility Tele where
--    getResult :: s -> Rep s -> Maybe Value
    getResult _ = _TR_result

--    getMsg :: s -> Upd s -> Maybe (Msg s)
    getMsg _ (TlUpdate _ (TEMsg m)) = Just m
    getMsg _ _ = Nothing

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


--    parseHTTPResponse :: s -> BSL.ByteString -> Either String (Rep s)
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

--    processMessage :: (Monad m) => D.Handle s m -> s -> Msg s -> m (Maybe (m H.HTTPRequest))
    processMessage h s m = either
        (\e -> D.logError h (T.pack e) >> return Nothing)
        (return . Just)
            $ sendMessage h s m
      where sendMessage h s m =
                let eithMethodParams = sendMessageTele m
                    url = tlUrl $ D.getConstState h s
                in  fmap (return . fmsg url . first TL.fromStrict) eithMethodParams


