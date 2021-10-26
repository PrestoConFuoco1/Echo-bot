{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
module DerivingJSON where

import qualified Data.Aeson.Types as Ae
import GHC.Generics
import Data.Char (isLower)


newtype BotSelectorModifier a = BotSelectorModifier { unBotSelectorModifier :: a }

instance (Generic a, Ae.GFromJSON Ae.Zero (Rep a)) => Ae.FromJSON (BotSelectorModifier a) where
    parseJSON = fmap BotSelectorModifier . Ae.genericParseJSON
        (Ae.defaultOptions {Ae.fieldLabelModifier = Ae.camelTo2 '_' . dropWhile isLower})

instance (Generic a, Ae.GToJSON' Ae.Value Ae.Zero (Rep a)) => Ae.ToJSON (BotSelectorModifier a) where
    toJSON = Ae.genericToJSON
        (Ae.defaultOptions {Ae.fieldLabelModifier = Ae.camelTo2 '_' . dropWhile isLower})
        . unBotSelectorModifier


