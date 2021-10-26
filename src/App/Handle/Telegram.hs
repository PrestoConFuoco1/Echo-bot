{-# LANGUAGE DataKinds #-}
module App.Handle.Telegram where

import qualified App.Handle as D
import qualified App.Logger as L
import BotClass.ClassTypesTeleInstance
import qualified Control.Monad.Catch as C
import Data.IORef
import qualified Data.Map as M
import qualified Data.Text as T (pack)
import qualified System.Exit as Q (ExitCode(..), exitWith)
import Telegram
import qualified Telegram.Exceptions as TlEx
import qualified Telegram.Send as G
import Types

data Config =
   Config
      { configCommonEnv :: EnvironmentCommon
      , configTelegram :: TlConfig
      }

data Resources =
   Resources
      { commonEnv :: EnvironmentCommon
      , constState :: TlStateConst
      , mutState :: IORef TlStateMut
      , usersMap :: IORef (M.Map TlUser Int)
      }

initResources :: L.Handle IO -> Config -> IO Resources
initResources _ (Config common tlConf) = do
   let initStateTele =
          TSM
             { stmUpdateID = configUpdateID tlConf
             , stmMediaGroups = M.empty
             }
       const_ = TSC {stcUrl = configUrl tlConf}
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
      Resources -> L.Handle IO -> D.Handle 'Telegram IO
resourcesToHandle resources logger =
   D.Handle
      { D.log = logger
      , D.commonEnv = commonEnv resources
      , D.getConstState = constState resources
      , D.insertUser =
           \u i ->
              modifyIORef'
                 (usersMap resources)
                 (M.insert u i)
      , D.getUser =
           \u ->
              M.lookup u <$> readIORef (usersMap resources)
      , D.getUpdates = G.getUpdates logger
      , D.sendEcho = G.sendThis logger
      , D.sendHelp = G.sendThis logger
      , D.sendKeyboard = G.sendThis logger
      , D.sendRepNumMessage = G.sendThis logger
      , D.specH =
           resourcesToTelegramHandler resources logger
      }

resourcesToTelegramHandler ::
      Resources -> L.Handle IO -> TlHandler IO
resourcesToTelegramHandler resources _ =
   TlHandler
      { getUpdateID =
           getUpdateIDPure <$>
           readIORef (mutState resources)
      , putUpdateID =
           modifyIORef' (mutState resources) . putUpdateIDPure
      , insertMediaGroupUnit =
           \k v ->
              modifyIORef' (mutState resources) $
              insertMediaGroupUnitPure k v
      , purgeMediaGroups =
           modifyIORef'
              (mutState resources)
              purgeMediaGroupsPure
      , getMediaGroups =
           getMediaGroupsPure <$>
           readIORef (mutState resources)
      }

tlErrorHandler ::
      L.Handle IO
   -> TlConfig
   -> Resources
   -> TlEx.TlException
   -> IO Resources
tlErrorHandler logger _ _ TlEx.TlException = do
   L.logFatal
      logger
      "Unable to handle any telegram errors, error is logged"
   Q.exitWith (Q.ExitFailure 1)

defaultHandler ::
      L.Handle IO
   -> Resources
   -> C.SomeException
   -> IO Resources
defaultHandler logger _ e = do
   L.logFatal logger "unable to handle exception"
   L.logFatal logger $ T.pack $ C.displayException e
   Q.exitWith (Q.ExitFailure 1)

tlHandlers ::
      L.Handle IO
   -> TlConfig
   -> Resources
   -> [C.Handler IO Resources]
tlHandlers logger conf resources =
   [C.Handler $ tlErrorHandler logger conf resources]
