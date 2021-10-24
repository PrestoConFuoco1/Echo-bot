module Telegram.ForHandlers where

import qualified Data.Map as M
import Telegram.General
import Telegram.MediaGroup.Types

getUpdateIDPure :: TlStateMut -> Integer
getUpdateIDPure = tlUpdateID

putUpdateIDPure :: Integer -> TlStateMut -> TlStateMut
putUpdateIDPure uid m = m {tlUpdateID = uid}

insertMediaGroupUnitPure ::
      TlMediaGroupIdentifier
   -> TlMediaGroupUnit
   -> TlStateMut
   -> TlStateMut
insertMediaGroupUnitPure key value sm =
   let newMap =
          M.insertWith (++) key [value] $ mediaGroups sm
    in sm {mediaGroups = newMap}

purgeMediaGroupsPure :: TlStateMut -> TlStateMut
purgeMediaGroupsPure sm = sm {mediaGroups = M.empty}

getMediaGroupsPure :: TlStateMut -> [TlMediaGroupPair]
getMediaGroupsPure TSM {mediaGroups = x} = map f $ M.toList x
  where
    f (k, v) = TlMediaGroupPair k v

data TlHandler m =
   TlHandler
      { getUpdateID :: m Integer
      , putUpdateID :: Integer -> m ()
      , insertMediaGroupUnit :: TlMediaGroupIdentifier -> TlMediaGroupUnit -> m ()
      , purgeMediaGroups :: m ()
      , getMediaGroups :: m [TlMediaGroupPair]
      }
