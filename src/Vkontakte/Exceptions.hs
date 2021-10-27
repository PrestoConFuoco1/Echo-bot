module Vkontakte.Exceptions where

import qualified Control.Monad.Catch as C

data VkException
  = KeyOutOfDateGetNew
  | KeyAndTsLosedGetNew
  deriving (Show, Eq)

instance C.Exception VkException
