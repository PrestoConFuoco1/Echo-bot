module Vkontakte.Exceptions where

import qualified Control.Monad.Catch as C

data VkException
   = KeyOutOfDate_GetNew
   | KeyAndTsLosed_GetNew
   deriving (Show, Eq)

instance C.Exception VkException
