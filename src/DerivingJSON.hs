{-# LANGUAGE DeriveGeneric, FlexibleContexts, UndecidableInstances, DerivingStrategies, DerivingVia #-}
module DerivingJSON where


import Data.Aeson.Types
import GHC.Generics
import qualified Stuff as S
import Data.Char (isLower)


newtype BotSelectorModifier a = BotSelectorModifier { unBotSelectorModifier :: a }
instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (BotSelectorModifier a) where
    parseJSON = fmap BotSelectorModifier . genericParseJSON
        (defaultOptions {fieldLabelModifier = camelTo2 '_' . dropWhile isLower})

instance (Generic a, GToJSON' Value Zero (Rep a)) => ToJSON (BotSelectorModifier a) where
    toJSON = genericToJSON
        (defaultOptions {fieldLabelModifier = camelTo2 '_' . dropWhile isLower})
        . unBotSelectorModifier


newtype RemovePrefix a = RemovePrefix a

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (RemovePrefix a) where
    parseJSON = fmap RemovePrefix . genericParseJSON
        (defaultOptions {fieldLabelModifier = S.removeTwoUnderscores})

