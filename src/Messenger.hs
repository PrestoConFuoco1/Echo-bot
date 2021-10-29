{-# LANGUAGE DerivingVia #-}

module Messenger (Messenger (..)) where

import qualified GenericPretty as GP

data Messenger
  = Vkontakte
  | Telegram
  deriving stock (Show, Eq)
  deriving (GP.PrettyShow) via GP.Showable Messenger
