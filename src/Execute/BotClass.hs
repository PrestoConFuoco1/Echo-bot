{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Execute.BotClass
    ( BotClassUtility(..)
    , BotClass(..)
    ) where

import qualified App.BotHandler as BotH
import BotTypesClass.ClassTypes
import qualified Control.Monad.Catch as C
import Data.Aeson (Value)
import qualified Data.Text as T (Text)
import qualified HTTP.Types as H

class (BotClassTypes s) => BotClassUtility s where
    getResult :: RepSucc s -> Maybe Value
    getMsg :: Upd s -> Maybe (Msg s)
    getUpdateValue :: Upd s -> Value
    getChat :: Msg s -> Maybe (Chat s)
    getUser :: Msg s -> Maybe (User s)
    getText :: Msg s -> Maybe T.Text
    getUserID :: User s -> T.Text
    getCallbackQuery :: Upd s -> Maybe (CallbackQuery s)
    getCallbackUser :: CallbackQuery s -> User s
    getCallbackData :: CallbackQuery s -> Maybe T.Text
    getCallbackChat :: CallbackQuery s -> Maybe (Chat s)

class (BotClassUtility s, BotH.HasBotHandler s) => BotClass s where
    takesJSON :: Bool
    getUpdatesRequest ::
           (Monad m) => BotH.BotHandler s m -> m H.HTTPRequest
    isSuccess :: Rep s -> Bool
    handleFailedUpdatesRequest ::
           (C.MonadThrow m) => BotH.BotHandler s m -> RepErr s -> m ()
    parseUpdatesValueList :: RepSucc s -> Either String [Value]
    parseUpdate :: Value -> Either String (Upd s)
    sendTextMsg ::
           (Monad m)
        => BotH.BotHandler s m
        -> Maybe (Chat s)
        -> Maybe (User s)
        -> T.Text
        -> m (Either String H.HTTPRequest)
    repNumKeyboard :: [Int] -> T.Text -> H.ParamsList
    processMessage ::
           (Monad m)
        => BotH.BotHandler s m
        -> Msg s
        -> m (Maybe (m H.HTTPRequest))
    epilogue ::
           (Monad m)
        => BotH.BotHandler s m
        -> [Upd s]
        -> RepSucc s
        -> m ()
