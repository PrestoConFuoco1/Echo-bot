module Vkontakte.ForHandlers where

import qualified Data.Text as T (Text)
import System.Random (randomR)
import Vkontakte.General

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

data VkHandler m =
   VkHandler
      { getRandomID :: m Integer
      , getTimestamp :: m T.Text
      , putTimestamp :: T.Text -> m ()
      }
