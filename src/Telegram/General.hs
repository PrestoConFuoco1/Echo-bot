{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Telegram.General where

import qualified Data.Map as M
import qualified Data.Text as T (Text)
import GHC.Generics (Generic)
import GenericPretty as GP
import Telegram.Types.MediaGroup (TlMediaGroupIdentifier, TlMediaGroupUnit)

tlTakesJSON :: Bool
tlTakesJSON = True -- this is better


