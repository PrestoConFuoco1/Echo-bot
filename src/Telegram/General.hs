{-# LANGUAGE DeriveGeneric #-}
module Telegram.General where

import GHC.Generics (Generic)
import qualified Data.Text.Lazy as TL (Text, unpack, pack)
import qualified Data.Text as T (Text, unpack, pack)

import GenericPretty as GP
import Telegram.ProcessMessage.Types
import Telegram.MediaGroup.Types
import qualified Data.Map as M



tlTakesJSON = True

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
    --photoMediaGroups :: M.Map TlMediaGroupIdentifier [TlPhotoSize]
    photoMediaGroups :: M.Map TlMediaGroupIdentifier [TlInputMediaPhoto]
    } deriving (Show)


