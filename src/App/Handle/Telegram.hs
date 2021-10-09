module App.Handle.Telegram where


import qualified App.Handle as D
import qualified App.Logger as L
import Telegram.Types
import BotClass.ClassTypesTeleInstance
import Types
import Data.IORef
import qualified Data.Map as M
import qualified HTTPRequests as H

data Config = Config {
      configCommonEnv :: EnvironmentCommon
    , configTelegram :: TlConfig
    }

data Resources = Resources {
      commonEnv :: EnvironmentCommon
    , constState :: TlStateConst
    , mutState :: IORef TlStateMut

    , usersMap :: IORef (M.Map TlUser Int)
    }



initResources :: L.Handle IO -> Config -> IO Resources
initResources logger (Config common tlConf) = do
    let initStateTele = TLSM { tlUpdateID = _TC_updID tlConf }
        const_ = TLSC { tlUrl = _TC_url tlConf }
    mut <- newIORef initStateTele
    umap <- newIORef M.empty
    return Resources {
          commonEnv = common
        , constState = const_
        , mutState = mut
        , usersMap = umap
        }


--initResources :: Config -> IO Resources
resourcesToHandle :: Resources -> L.Handle IO -> D.Handle Tele IO
resourcesToHandle resources logger =
    D.Handle {
          D.log = logger
        , D.sendRequest = H.sendRequest logger
        , D.commonEnv = commonEnv resources
        , D.getConstState = const (constState resources)
--        , D.getMutState = const (readIORef $ mutState resources)
--        , D.putMutState = const (writeIORef $ mutState resources)

        , D.insertUser = \u i -> modifyIORef' (usersMap resources) (M.insert u i)
        , D.getUser = \u -> readIORef (usersMap resources) >>= return . M.lookup u

        , D.specH = resourcesToTelegramHandler resources logger
    }

resourcesToTelegramHandler :: Resources -> L.Handle IO -> TlHandler IO
resourcesToTelegramHandler resources logger =
    TlHandler {
          getUpdateID = fmap getUpdateID' $ readIORef (mutState resources)
        , putUpdateID = modifyIORef' (mutState resources) . putUpdateID'
    }

