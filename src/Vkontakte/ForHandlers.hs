module Vkontakte.ForHandlers where

import qualified Data.Text as T (Text)
import System.Random (randomR)
import Vkontakte.General

getRandomID' :: VkStateMut -> (VkStateMut, Integer)
getRandomID' sm =
   let (rndInt32, g') =
          randomR (0 :: Integer, 2 ^ (32 - 1 :: Int)) $
          vkRndGen sm
    in (sm {vkRndGen = g'}, rndInt32)

putTimestamp' :: T.Text -> VkStateMut -> VkStateMut
putTimestamp' newTs sm = sm {vkTs = newTs}

getTimestamp' :: VkStateMut -> T.Text
getTimestamp' = vkTs

data VkHandler m =
   VkHandler
      { getRandomID :: m Integer
      , getTimestamp :: m T.Text
      , putTimestamp :: T.Text -> m ()
      }
--------------------------------------------
