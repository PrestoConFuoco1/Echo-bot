{-# LANGUAGE DeriveGeneric #-}
module Telegram.General where

import GHC.Generics (Generic)
import qualified Data.Text.Lazy as TL (Text)
import GenericPretty as GP
import Telegram.MediaGroup.Types
import qualified Data.Map as M


tlTakesJSON :: Bool
tlTakesJSON = True -- this is better
--tlTakesJSON = False


data TlConfig = TlConf {
        _TC_updID :: Integer,
        _TC_url :: TL.Text
    } deriving (Show, Generic)
instance PrettyShow TlConfig


data TlStateConst = TLSC {
    tlUrl :: TL.Text
    } deriving (Show)

data TlStateMut = TLSM {
    tlUpdateID :: Integer,
    mediaGroups :: M.Map TlMediaGroupIdentifier [TlMediaGroupUnit]
    } deriving (Show)


