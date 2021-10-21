module Vkontakte.ForHandlers where

import qualified Data.Text.Lazy as TL (Text)
import System.Random (randomR)
import Vkontakte.General

getRandomID' :: VkStateMut -> (VkStateMut, Integer)
getRandomID' sm =
   let (rndInt32, g') =
          randomR (0 :: Integer, 2 ^ (32 - 1 :: Int)) $
          vkRndGen sm
    in (sm {vkRndGen = g'}, rndInt32)

putTimestamp' :: TL.Text -> VkStateMut -> VkStateMut
putTimestamp' newTs sm = sm {vkTs = newTs}

getTimestamp' :: VkStateMut -> TL.Text
getTimestamp' = vkTs

data VkHandler m =
   VkHandler
      { getRandomID :: m Integer
      , getTimestamp :: m TL.Text
      , putTimestamp :: TL.Text -> m ()
      }
--------------------------------------------
