module Telegram.ForHandlers where

import qualified Data.Map as M
import Telegram.General (TlStateMut (..))
import Telegram.Types.MediaGroup (TlMediaGroupIdentifier, TlMediaGroupPair (..), TlMediaGroupUnit)

getUpdateIDPure :: TlStateMut -> Integer
getUpdateIDPure = stmUpdateID

putUpdateIDPure :: Integer -> TlStateMut -> TlStateMut
putUpdateIDPure uid m = m {stmUpdateID = uid}

insertMediaGroupUnitPure ::
  TlMediaGroupIdentifier ->
  TlMediaGroupUnit ->
  TlStateMut ->
  TlStateMut
insertMediaGroupUnitPure key value sm =
  let newMap =
        M.insertWith (++) key [value] $ stmMediaGroups sm
   in sm {stmMediaGroups = newMap}

purgeMediaGroupsPure :: TlStateMut -> TlStateMut
purgeMediaGroupsPure sm = sm {stmMediaGroups = M.empty}

getMediaGroupsPure :: TlStateMut -> [TlMediaGroupPair]
getMediaGroupsPure TSM {stmMediaGroups = x} = map f $ M.toList x
  where
    f (k, v) = TlMediaGroupPair k v

data TlHandler m = TlHandler
  { getUpdateID :: m Integer,
    putUpdateID :: Integer -> m (),
    insertMediaGroupUnit :: TlMediaGroupIdentifier -> TlMediaGroupUnit -> m (),
    purgeMediaGroups :: m (),
    getMediaGroups :: m [TlMediaGroupPair]
  }
