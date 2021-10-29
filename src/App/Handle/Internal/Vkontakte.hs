{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module App.Handle.Internal.Vkontakte where

import qualified Data.Text as T (Text)
import GHC.Generics (Generic)
import GenericPretty (PrettyShow)
import System.Random (randomR, StdGen)


data VkStateConst = VKSC
  { vkKey :: T.Text,
    vkServer :: T.Text,
    vkUrl :: T.Text, -- only for methods
    vkAccessToken :: T.Text,
    vkGroupID :: Integer,
    apiVersion :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyShow)

data VkStateMut = VKSM
  { vkTs :: T.Text, -- timestamp
    vkRndGen :: StdGen
  }
  deriving stock (Show)

getRandomIDPure :: VkStateMut -> (VkStateMut, Integer)
getRandomIDPure sm =
  let (rndInt32, g') =
        randomR (0 :: Integer, 2 ^ (32 - 1 :: Int)) $
          vkRndGen sm
   in (sm {vkRndGen = g'}, rndInt32)

putTimestampPure :: T.Text -> VkStateMut -> VkStateMut
putTimestampPure newTs sm = sm {vkTs = newTs}

getTimestampPure :: VkStateMut -> T.Text
getTimestampPure = vkTs

data VkHandler m = VkHandler
  { getRandomID :: m Integer,
    getTimestamp :: m T.Text,
    putTimestamp :: T.Text -> m ()
  }
