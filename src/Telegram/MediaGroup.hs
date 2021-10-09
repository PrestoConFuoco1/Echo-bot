{-# LANGUAGE
    DeriveGeneric
    #-}
module Telegram.MediaGroup where

import GHC.Generics
import qualified GenericPretty as GP
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Aeson as Ae (encode, ToJSON(..))
import Telegram.Entity
import qualified App.Handle as D
--import App.Handle.Telegram
import qualified Stuff as S (safeHead, withMaybe)
import Telegram.ProcessMessage.Types
import Data.Foldable (asum)
import Telegram.MediaGroup.Types
import BotClass.ClassTypesTeleInstance
import Telegram.ForHandlers
import BotClass.Class
import HTTPRequests as H

