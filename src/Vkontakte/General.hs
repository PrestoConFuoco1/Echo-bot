{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Vkontakte.General where

import qualified Data.Text as T (Text)
import GHC.Generics (Generic)
import GenericPretty (PrettyShow)
import HTTPTypes as H
import System.Random (StdGen)

defaultVkParams' :: T.Text -> T.Text -> H.ParamsList
defaultVkParams' accTok apiV =
  [unit "access_token" accTok, unit "v" apiV]

vkTakesJSON :: Bool
vkTakesJSON = False


