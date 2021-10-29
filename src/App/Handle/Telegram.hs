{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module App.Handle.Telegram (
    TlHandler(..), TlStateConst (..), initResources, resourcesToHandle, tlErrorHandlers, Resources (..)
) where

import GHC.Generics
import GenericPretty as GP
import qualified App.Handle as D
import qualified App.Logger as L
import BotTypesClass.TelegramInstance ()
import qualified Control.Monad.Catch as C
import Data.IORef
import qualified Data.Map as M
import qualified Data.Text as T (pack, Text)
import qualified System.Exit as Q (ExitCode (..), exitWith)
import Telegram (TlMediaGroupUnit, TlMediaGroupIdentifier, TlMediaGroupPair(..), TlUser)
import qualified Telegram.Exceptions as TlEx
import qualified HTTP.Telegram as HTl
import qualified Messenger as M
import Config.Types (TlConfig(..))
import qualified Environment as Env

instance D.HasBotHandler 'M.Telegram where
    type StateC 'M.Telegram = TlStateConst
    type StateM 'M.Telegram = TlStateMut
    type Hndl 'M.Telegram = TlHandler

data Resources = Resources
  { commonEnv :: Env.EnvironmentCommon,
    constState :: TlStateConst,
    mutState :: IORef TlStateMut,
    usersMap :: IORef (M.Map TlUser Int)
  }

data TlHandler m = TlHandler
  { getUpdateID :: m Integer,
    putUpdateID :: Integer -> m (),
    insertMediaGroupUnit :: TlMediaGroupIdentifier -> TlMediaGroupUnit -> m (),
    purgeMediaGroups :: m (),
    getMediaGroups :: m [TlMediaGroupPair]
  }

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


----------------- IO handler ------------------------

initResources :: L.LoggerHandler IO -> Env.EnvironmentCommon -> TlConfig -> IO Resources
initResources _ common tlConf = do
  let initStateTele =
        TSM
          { stmUpdateID = tlConfigUpdateID tlConf,
            stmMediaGroups = M.empty
          }
      const_ = TSC {stcUrl = tlConfigUrl tlConf}
  mut <- newIORef initStateTele
  umap <- newIORef M.empty
  pure
    Resources
      { commonEnv = common,
        constState = const_,
        mutState = mut,
        usersMap = umap
      }

resourcesToHandle ::
  Resources -> L.LoggerHandler IO -> D.BotHandler 'M.Telegram IO
resourcesToHandle resources logger =
  D.BotHandler
    { D.log = logger,
      D.commonEnv = commonEnv resources,
      D.getConstState = constState resources,
      D.insertUser =
        \u i ->
          modifyIORef'
            (usersMap resources)
            (M.insert u i),
      D.getUser =
        \u ->
          M.lookup u <$> readIORef (usersMap resources),
      D.getUpdates = HTl.getUpdates logger,
      D.sendEcho = HTl.sendThis logger,
      D.sendHelp = HTl.sendThis logger,
      D.sendKeyboard = HTl.sendThis logger,
      D.sendRepNumMessage = HTl.sendThis logger,
      D.specH =
        resourcesToTelegramHandler resources logger
    }

resourcesToTelegramHandler ::
  Resources -> L.LoggerHandler IO -> TlHandler IO
resourcesToTelegramHandler resources _ =
  TlHandler
    { getUpdateID =
        getUpdateIDPure
          <$> readIORef (mutState resources),
      putUpdateID =
        modifyIORef' (mutState resources) . putUpdateIDPure,
      insertMediaGroupUnit =
        \k v ->
          modifyIORef' (mutState resources) $
            insertMediaGroupUnitPure k v,
      purgeMediaGroups =
        modifyIORef'
          (mutState resources)
          purgeMediaGroupsPure,
      getMediaGroups =
        getMediaGroupsPure
          <$> readIORef (mutState resources)
    }

tlErrorHandler ::
  L.LoggerHandler IO ->
  TlConfig ->
  Resources ->
  TlEx.TlException ->
  IO Resources
tlErrorHandler logger _ _ TlEx.TlException = do
  L.logFatal
    logger
    "Unable to handle any telegram errors, error is logged"
  Q.exitWith (Q.ExitFailure 1)

defaultHandler ::
  L.LoggerHandler IO ->
  Resources ->
  C.SomeException ->
  IO Resources
defaultHandler logger _ e = do
  L.logFatal logger "unable to handle exception"
  L.logFatal logger $ T.pack $ C.displayException e
  Q.exitWith (Q.ExitFailure 1)

tlErrorHandlers ::
  L.LoggerHandler IO ->
  TlConfig ->
  Resources ->
  [C.Handler IO Resources]
tlErrorHandlers logger conf resources =
  [C.Handler $ tlErrorHandler logger conf resources]


------ pure functions -------------
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


