{-# LANGUAGE TypeFamilies, FlexibleContexts, DataKinds #-}
module BotClass.ClassTypes where

import qualified Types as Y
import qualified GenericPretty as GP

class ( GP.PrettyShow (Rep s)
      , Show (Rep s)
      , GP.PrettyShow (RepSucc s)
      , GP.PrettyShow (RepErr s)
      , GP.PrettyShow (Msg s)
      , GP.PrettyShow (Conf s)
      ) =>
      BotClassTypes (s :: Y.Messenger)
   where
   type Conf s :: *
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
