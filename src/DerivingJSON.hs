{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module DerivingJSON where

import qualified Data.Aeson.Types as Ae
import Data.Char (isLower)
import GHC.Generics

newtype BotSelectorModifier a = BotSelectorModifier {unBotSelectorModifier :: a}

instance (Generic a, Ae.GFromJSON Ae.Zero (Rep a)) => Ae.FromJSON (BotSelectorModifier a) where
  parseJSON =
    fmap BotSelectorModifier
      . Ae.genericParseJSON
        (Ae.defaultOptions {Ae.fieldLabelModifier = Ae.camelTo2 '_' . dropWhile isLower})

instance (Generic a, Ae.GToJSON' Ae.Value Ae.Zero (Rep a)) => Ae.ToJSON (BotSelectorModifier a) where
  toJSON =
    Ae.genericToJSON
      (Ae.defaultOptions {Ae.fieldLabelModifier = Ae.camelTo2 '_' . dropWhile isLower})
      . unBotSelectorModifier
