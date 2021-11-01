{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module App.Handle.Telegram
    ( TlHandler(..)
    , TlStateConst(..)
    , initResources
    , resourcesToHandle
    , tlErrorHandlers
    , Resources(..)
    ) where

import qualified App.BotHandler as BotH
import qualified App.Logger as L
import BotTypesClass.TelegramInstance ()
import Config.Types (TlConfig(..))
import qualified Control.Monad.Catch as C
import Data.IORef
import qualified Data.Map as M
import qualified Data.Text as T (Text)
import qualified Environment as Env
import GHC.Generics
import GenericPretty as GP
import qualified HTTP.Telegram as HTl
import qualified Messenger as M
import qualified System.Exit as Q (ExitCode(..), exitWith)
import Telegram
    ( TlMediaGroupIdentifier
    , TlMediaGroupPair(..)
    , TlMediaGroupUnit
    , TlUser
    )
import qualified Telegram.Exceptions as TlEx

instance BotH.HasBotHandler 'M.Telegram where
    type StateC 'M.Telegram = TlStateConst
    type StateM 'M.Telegram = TlStateMut
    type Hndl 'M.Telegram = TlHandler

data Resources =
    Resources
        { commonEnv :: Env.Environment
        , constState :: TlStateConst
        , mutState :: IORef TlStateMut
        , usersMap :: IORef (M.Map TlUser Int)
        }

data TlHandler m =
    TlHandler
        { getUpdateID :: m Integer
        , putUpdateID :: Integer -> m ()
        , insertMediaGroupUnit :: TlMediaGroupIdentifier -> TlMediaGroupUnit -> m ()
        , purgeMediaGroups :: m ()
        , getMediaGroups :: m [TlMediaGroupPair]
        }

newtype TlStateConst =
    TSC
        { stcUrl :: T.Text
        }
  deriving (Show, Generic)
  deriving anyclass (PrettyShow)

data TlStateMut =
    TSM
        { stmUpdateID :: Integer
        , stmMediaGroups :: M.Map TlMediaGroupIdentifier [TlMediaGroupUnit]
        }
  deriving (Show, Generic)

----------------- IO handler ------------------------
initResources ::
       L.LoggerHandler IO
    -> Env.Environment
    -> TlConfig
    -> IO Resources
initResources _ common tlConf = do
    let initStateTele =
            TSM
                { stmUpdateID = tlConfigUpdateID tlConf
                , stmMediaGroups = M.empty
                }
        const_ = TSC {stcUrl = tlConfigUrl tlConf}
    mut <- newIORef initStateTele
    umap <- newIORef M.empty
    pure
        Resources
            { commonEnv = common
            , constState = const_
            , mutState = mut
            , usersMap = umap
            }

resourcesToHandle ::
       Resources -> L.LoggerHandler IO -> BotH.BotHandler 'M.Telegram IO
resourcesToHandle resources logger =
    BotH.BotHandler
        { BotH.log = logger
        , BotH.commonEnv = commonEnv resources
        , BotH.getConstState = constState resources
        , BotH.insertUser =
              \u i -> modifyIORef' (usersMap resources) (M.insert u i)
        , BotH.getUser =
              \u -> M.lookup u <$> readIORef (usersMap resources)
        , BotH.getUpdates = HTl.getUpdates logger
        , BotH.sendEcho = HTl.sendThis logger
        , BotH.sendHelp = HTl.sendThis logger
        , BotH.sendKeyboard = HTl.sendThis logger
        , BotH.sendRepNumMessage = HTl.sendThis logger
        , BotH.specH = resourcesToTelegramHandler resources logger
        }

resourcesToTelegramHandler ::
       Resources -> L.LoggerHandler IO -> TlHandler IO
resourcesToTelegramHandler resources _ =
    TlHandler
        { getUpdateID =
              getUpdateIDPure <$> readIORef (mutState resources)
        , putUpdateID =
              modifyIORef' (mutState resources) . putUpdateIDPure
        , insertMediaGroupUnit =
              \k v ->
                  modifyIORef' (mutState resources) $
                  insertMediaGroupUnitPure k v
        , purgeMediaGroups =
              modifyIORef' (mutState resources) purgeMediaGroupsPure
        , getMediaGroups =
              getMediaGroupsPure <$> readIORef (mutState resources)
        }

tlErrorHandler ::
       L.LoggerHandler IO
    -> TlConfig
    -> Resources
    -> TlEx.TlException
    -> IO Resources
tlErrorHandler logger _ _ TlEx.TlException = do
    L.logFatal
        logger
        "Unable to handle any telegram errors, error is logged"
    Q.exitWith (Q.ExitFailure 1)

tlErrorHandlers ::
       L.LoggerHandler IO
    -> TlConfig
    -> Resources
    -> [C.Handler IO Resources]
tlErrorHandlers logger conf resources =
    [C.Handler $ tlErrorHandler logger conf resources]

------ pure functions -------------
getUpdateIDPure :: TlStateMut -> Integer
getUpdateIDPure = stmUpdateID

putUpdateIDPure :: Integer -> TlStateMut -> TlStateMut
putUpdateIDPure uid m = m {stmUpdateID = uid}

insertMediaGroupUnitPure ::
       TlMediaGroupIdentifier
    -> TlMediaGroupUnit
    -> TlStateMut
    -> TlStateMut
insertMediaGroupUnitPure key value sm =
    let newMap = M.insertWith (++) key [value] $ stmMediaGroups sm
     in sm {stmMediaGroups = newMap}

purgeMediaGroupsPure :: TlStateMut -> TlStateMut
purgeMediaGroupsPure sm = sm {stmMediaGroups = M.empty}

getMediaGroupsPure :: TlStateMut -> [TlMediaGroupPair]
getMediaGroupsPure TSM {stmMediaGroups = x} = map f $ M.toList x
  where
    f (k, v) = TlMediaGroupPair k v
