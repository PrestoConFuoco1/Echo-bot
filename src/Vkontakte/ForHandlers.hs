{-# LANGUAGE
    DeriveGeneric,
    RecordWildCards
    #-}

module Vkontakte.ForHandlers where

import Data.Aeson (decode)
import Data.Aeson.Types
import GHC.Generics (Generic)
import System.Random (StdGen, randomR)
import qualified Data.Text.Lazy as TL (Text, unpack, pack, toStrict)
import qualified Data.Text as T (Text, unpack, pack)
import qualified Data.ByteString.Lazy as BSL (ByteString)

import GenericPretty

import Vkontakte.Entity as VE
import Vkontakte.Attachment as VA
import Vkontakte.Keyboard as VKb
import HTTPRequests as H
import Vkontakte.General

getRandomID' :: VkStateMut -> (VkStateMut, Integer)
getRandomID' sm =
    let (rndInt32, g') = randomR (0::Integer, 2^32-1) $ vkRndGen sm
    in  (sm { vkRndGen = g' }, rndInt32)

putTimestamp' :: TL.Text -> VkStateMut -> VkStateMut
putTimestamp' newTs sm = sm { vkTs = newTs }

getTimestamp' :: VkStateMut -> TL.Text
getTimestamp' = vkTs

data VkHandler m = VkHandler {
    getRandomID :: m Integer,
    getTimestamp :: m TL.Text,
    putTimestamp :: TL.Text -> m ()
    }

--------------------------------------------



