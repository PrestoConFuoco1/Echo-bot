module Vkontakte
  ( module V,
    defaultVkParams',
    vkTakesJSON
  )
where

import Vkontakte.Attachment as V
import Vkontakte.Entity as V
import Vkontakte.Exceptions as V
import Vkontakte.Keyboard as V
import Vkontakte.Update as V

import qualified Data.Text as T (Text)
import HTTP.Types as H



defaultVkParams' :: T.Text -> T.Text -> H.ParamsList
defaultVkParams' accTok apiV =
  [unit "access_token" accTok, unit "v" apiV]

vkTakesJSON :: Bool
vkTakesJSON = False


