{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Telegram.General where

import qualified Data.Map as M
import qualified Data.Text as T (Text)
import GHC.Generics (Generic)
import GenericPretty as GP
import Telegram.MediaGroup.Types (TlMediaGroupIdentifier, TlMediaGroupUnit)

tlTakesJSON :: Bool
tlTakesJSON = True -- this is better

data TlConfig =
   TlConf
      { configUpdateID :: Integer
      , configUrl :: T.Text
      }
    deriving stock (Show, Generic)
    deriving anyclass PrettyShow

newtype TlStateConst =
   TSC
      { stcUrl :: T.Text
      }
    deriving stock (Show, Generic)
    deriving anyclass PrettyShow


data TlStateMut =
   TSM
      { stmUpdateID :: Integer
      , stmMediaGroups :: M.Map TlMediaGroupIdentifier [TlMediaGroupUnit]
      }
    deriving stock (Show, Generic)

