{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module BotTypesClass.ClassTypes where

import qualified GenericPretty as GP
import qualified Types as Y
import qualified Messenger as M

class
  ( GP.PrettyShow (Rep s),
    Show (Rep s),
    GP.PrettyShow (RepSucc s),
    GP.PrettyShow (RepErr s),
    GP.PrettyShow (Msg s)
  ) =>
  BotClassTypes (s :: M.Messenger)
  where
  type StateC s :: *
  type StateM s :: *
  type Rep s :: *
  type RepSucc s :: *
  type RepErr s :: *
  type Upd s :: *
  type Msg s :: *
  type Chat s :: *
  type User s :: *
  type CallbackQuery s :: *
  type Hndl s :: (* -> *) -> *
