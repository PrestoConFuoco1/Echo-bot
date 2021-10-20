{-# LANGUAGE
    TypeFamilies
    #-}

module BotClass.Class where

import qualified HTTPRequests as H
import Data.Aeson (Value)
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as TL (Text)
import qualified App.Handle as D
import BotClass.ClassTypes
import qualified Control.Monad.Catch as C

fmsg :: TL.Text -> (TL.Text, H.ParamsList) -> H.HTTPRequest
fmsg url (method, params) = H.Req H.POST (url <> method) params

class (BotClassTypes s) => BotClassUtility s where
    getResult :: s -> RepSucc s -> Maybe Value

    getMsg :: s -> Upd s -> Maybe (Msg s)
    getUpdateValue :: s -> Upd s -> Value

    getChat :: s -> Msg s -> Maybe (Chat s)
    getUser :: s -> Msg s -> Maybe (User s)
    getText :: s -> Msg s -> Maybe T.Text
    getUserID :: s -> User s -> T.Text

    getCallbackQuery :: s -> Upd s -> Maybe (CallbackQuery s)

    getCallbackUser :: s -> CallbackQuery s -> User s
    getCallbackData :: s -> CallbackQuery s -> Maybe T.Text
    getCallbackChat :: s -> CallbackQuery s -> Maybe (Chat s)


class (BotClassUtility s) => BotClass s where
    takesJSON :: s -> Bool
    getUpdatesRequest :: (Monad m) => D.Handle s m -> s -> m H.HTTPRequest
    isSuccess :: s -> Rep s -> Bool
    handleFailedUpdatesRequest :: (C.MonadThrow m) => D.Handle s m -> RepErr s -> m ()
    parseUpdatesValueList :: s -> RepSucc s -> Either String [Value]
    parseUpdate :: s -> Value -> Either String (Upd s)
    sendTextMsg :: (Monad m) => D.Handle s m -> s -> Maybe (Chat s) -> Maybe (User s) -> T.Text
        -> m (Either String H.HTTPRequest)

    repNumKeyboard :: s -> [Int] -> TL.Text -> H.ParamsList
    processMessage :: (Monad m) => D.Handle s m -> s -> Msg s -> m (Maybe (m H.HTTPRequest))
    epilogue :: (Monad m) => D.Handle s m -> s -> [Upd s] -> RepSucc s -> m ()
