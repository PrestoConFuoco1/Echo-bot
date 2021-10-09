{-# LANGUAGE TypeFamilies,
    FlexibleContexts,
    ConstrainedClassMethods,
    GeneralizedNewtypeDeriving,
    OverloadedStrings #-}

module BotClass.ClassTypes where


import qualified HTTPRequests as H
import qualified Stuff as S (Timeout)
import Data.Aeson (Value)
import qualified Data.Text as T (Text)
import qualified Data.ByteString.Lazy.Char8 as BSL (ByteString)
import qualified GenericPretty as GP





class (GP.PrettyShow (Rep s), Show (Rep s), GP.PrettyShow (Msg s),
        GP.PrettyShow (Conf s)) =>
        BotClassTypes s where

    type Conf s :: *


    type StateC s :: *
    type StateM s :: *

    type Rep s :: *
    type Upd s :: *
    type Msg s :: *
    type Chat s :: *
    type User s :: *

    type CallbackQuery s :: *

    type Hndl s :: (* -> *) -> *


