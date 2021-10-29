{-# LANGUAGE DerivingVia #-}

module Execute.Types
  ( Command (..),
    Event (..),
    CallbQuery (..),
  )
where

import Data.Aeson.Types (Value)

-------------------------------------------------------------------
data Command
  = Help
  | SetRepNum
  deriving (Show, Eq)

data Event h u m b
  = ECommand Command (Maybe h) (Maybe u)
  | EMessage m
  | ECallback b
  | EError Value

data CallbQuery u c
  = CSetRepNum u (Maybe c) Int
  | CError String

instance Show (Event h u m b) where
  show ECommand {} = "Command"
  show (EMessage _) = "Message"
  show (ECallback _) = "Callback"
  show _ = "Unexpected event"
