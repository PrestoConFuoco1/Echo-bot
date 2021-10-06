{-# LANGUAGE TypeFamilies,
    FlexibleContexts,
    ConstrainedClassMethods,
    GeneralizedNewtypeDeriving,
    OverloadedStrings #-}

module BotClass where

import qualified HTTPRequests as H
import qualified Stuff as S (Timeout)
import Data.Aeson (Value)
import qualified Data.Text as T (Text)
import qualified App.Handle as D
import qualified Data.ByteString.Lazy.Char8 as BSL (ByteString)
import qualified GenericPretty as GP


class (GP.PrettyShow (Rep s), Show (Rep s)) =>
        BotClass s where
    takesJSON :: s -> Bool

    type Conf s :: *


    type StC s :: *
    type StM s :: *

    type Rep s :: *
    type Upd s :: *
    type Msg s :: *
    type Chat s :: *
    type User s :: *

    type CallbackQuery s :: *


    getUpdatesRequest :: (Monad m) => D.Handle m -> s -> m H.HTTPRequest
    parseHTTPResponse :: s -> BSL.ByteString -> Either String (Rep s)

    isSuccess :: s -> Rep s -> Bool
    parseUpdatesList :: s -> Rep s -> Either String [Upd s]
    defaultStateTrans :: D.Handle m -> s -> [Upd s] -> Rep s -> m ()

    getMsg :: s -> Upd s -> Maybe (Msg s)

    getChat :: s -> Msg s -> Maybe (Chat s)
    getUser :: s -> Msg s -> Maybe (User s)
    getText :: s -> Msg s -> Maybe T.Text
    getUserID :: s -> User s -> T.Text

    getCallbackQuery :: s -> Upd s -> Maybe (CallbackQuery s)

--    sendTextMsg :: s -> Maybe (Chat s) -> Maybe (User s) -> T.Text
--        -> StC s -> Either String (State (StM s) H.HTTPRequest)
    sendTextMsg :: D.Handle m -> s -> Maybe (Chat s) -> Maybe (User s) -> T.Text
        -> m (Either String H.HTTPRequest)

    repNumKeyboard :: s -> [Int] -> T.Text -> H.ParamsList

