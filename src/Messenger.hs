{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
module Messenger where

import qualified GenericPretty as GP

data Messenger
  = Vkontakte
  | Telegram
  deriving stock (Show, Eq)
  deriving (GP.PrettyShow) via GP.Showable Messenger


