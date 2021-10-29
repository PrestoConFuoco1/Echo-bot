module Telegram
  ( module T,
    tlTakesJSON
  )
where

import Telegram.Exceptions as T
import Telegram.Keyboard as T
import Telegram.ProcessMessage as T
import Telegram.Types.Entity as T
import Telegram.Types.MediaGroup as T
import Telegram.Update as T

tlTakesJSON :: Bool
tlTakesJSON = True -- this is better


