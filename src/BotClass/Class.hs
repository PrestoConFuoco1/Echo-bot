{-# LANGUAGE TypeFamilies,
    FlexibleContexts,
    ConstrainedClassMethods,
    GeneralizedNewtypeDeriving,
    OverloadedStrings #-}

module BotClass.Class where

import qualified HTTPRequests as H
import qualified Stuff as S (Timeout)
import Data.Aeson (Value)
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as TL (Text)
import qualified App.Handle as D
import qualified Data.ByteString.Lazy.Char8 as BSL (ByteString)
import qualified GenericPretty as GP
import BotClass.ClassTypes


fmsg url (method, params) = H.Req H.POST (url <> method) params

class (BotClassTypes s) => BotClass s where
    takesJSON :: s -> Bool
    
    getUpdatesRequest :: (Monad m) => D.Handle s m -> s -> m H.HTTPRequest
    parseHTTPResponse :: s -> BSL.ByteString -> Either String (Rep s)

    isSuccess :: s -> Rep s -> Bool
    getResult :: s -> Rep s -> Maybe Value

    parseUpdatesList :: s -> Rep s -> Either String [Upd s]

    getMsg :: s -> Upd s -> Maybe (Msg s)

    getChat :: s -> Msg s -> Maybe (Chat s)
    getUser :: s -> Msg s -> Maybe (User s)
    getText :: s -> Msg s -> Maybe T.Text
    getUserID :: s -> User s -> T.Text

    getCallbackQuery :: s -> Upd s -> Maybe (CallbackQuery s)

    getCallbackUser :: s -> CallbackQuery s -> User s
    getCallbackData :: s -> CallbackQuery s -> Maybe T.Text
    getCallbackChat :: s -> CallbackQuery s -> Maybe (Chat s)

    sendTextMsg :: (Monad m) => D.Handle s m -> s -> Maybe (Chat s) -> Maybe (User s) -> T.Text
        -> m (Either String H.HTTPRequest)

    repNumKeyboard :: s -> [Int] -> TL.Text -> H.ParamsList

    processMessage :: (Monad m) => D.Handle s m -> s -> Msg s -> m (Maybe (m H.HTTPRequest))

    epilogue :: (Monad m) => D.Handle s m -> s -> [Upd s] -> Rep s -> m ()
