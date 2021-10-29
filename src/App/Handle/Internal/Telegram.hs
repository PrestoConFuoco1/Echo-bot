{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module App.Handle.Internal.Telegram where

import qualified Data.Text as T (Text)
import GHC.Generics (Generic)
import GenericPretty as GP
import qualified Data.Map as M
import Telegram.Types.MediaGroup (TlMediaGroupIdentifier, TlMediaGroupPair (..), TlMediaGroupUnit)




newtype TlStateConst = TSC
  { stcUrl :: T.Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (PrettyShow)

data TlStateMut = TSM
  { stmUpdateID :: Integer,
    stmMediaGroups :: M.Map TlMediaGroupIdentifier [TlMediaGroupUnit]
  }
  deriving stock (Show, Generic)

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
