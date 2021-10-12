module App.Handle.Telegram where


import qualified App.Handle as D
import qualified App.Logger as L
import Telegram.Types
import BotClass.ClassTypesTeleInstance
import Types
import Data.IORef
import qualified Data.Map as M
import qualified HTTPRequests as H
import qualified Data.Map as M (empty)
import qualified System.Exit as Q (ExitCode (..), exitWith)
import qualified Control.Monad.Catch as C
import qualified Telegram.Exceptions as TlEx
import qualified Data.Text as T

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
    let initStateTele = TLSM { tlUpdateID = _TC_updID tlConf, photoMediaGroups = M.empty }
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
        , D.getConstState = constState resources

        , D.insertUser = \u i -> modifyIORef' (usersMap resources) (M.insert u i)
        , D.getUser = \u -> readIORef (usersMap resources) >>= return . M.lookup u

        , D.specH = resourcesToTelegramHandler resources logger
    }

resourcesToTelegramHandler :: Resources -> L.Handle IO -> TlHandler IO
resourcesToTelegramHandler resources logger =
    TlHandler {
          getUpdateID = fmap getUpdateID' $ readIORef (mutState resources)
        , putUpdateID = modifyIORef' (mutState resources) . putUpdateID'
        , insertMediaGroupPhoto = \k v -> modifyIORef' (mutState resources) $ insertMediaGroupPhoto' k v
        , purgeMediaGroups = modifyIORef' (mutState resources) $ purgeMediaGroups'
        , getMediaGroups = fmap getMediaGroups' $ readIORef (mutState resources)
    }



tlErrorHandler :: L.Handle IO -> TlConfig -> Resources -> TlEx.TlException -> IO Resources
tlErrorHandler logger conf resources TlEx.TlException = do
    L.logFatal logger "Unable to handle any telegram errors, error is logged"
    Q.exitWith (Q.ExitFailure 1)


defaultHandler :: L.Handle IO -> Resources -> C.SomeException -> IO Resources
defaultHandler logger resources e = do
    L.logFatal logger $ "unable to handle exception"
    L.logFatal logger $ T.pack $ C.displayException e
    Q.exitWith (Q.ExitFailure 1)



tlHandlers logger conf resources = [
    C.Handler $ tlErrorHandler logger conf resources
    , C.Handler $ defaultHandler logger resources
    ]


