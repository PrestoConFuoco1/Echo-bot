module App.Handle.Vkontakte where

import qualified App.Handle as D
import qualified App.Logger as L
import Vkontakte.Types
import Vkontakte.Initialize
--import Vkontakte.Entity
import BotClass.ClassTypesVkInstance
import Types
import Data.IORef
import qualified Data.Map as M
import qualified HTTPRequests as H

import System.Random (StdGen, newStdGen, randomR)
import qualified Stuff as S
import qualified System.Exit as Q (ExitCode (..), exitWith)
import qualified Data.Text as T


data Config = Config {
      configCommonEnv :: EnvironmentCommon
    , configVkontakte :: VkConfig
    }

data Resources = Resources {
      commonEnv :: EnvironmentCommon
    , constState :: VkStateConst
    , mutState :: IORef VkStateMut

    , usersMap :: IORef (M.Map VkUser Int)
    }




initResources :: L.Handle IO -> Config -> IO Resources
initResources h (Config common vkConf) = do
    (sc, sm) <- initialize h vkConf
    umap <- newIORef M.empty
    mut <- newIORef sm
    return Resources {
        commonEnv = common,
        constState = sc,
        mutState = mut,
        usersMap = umap
        }

resourcesToHandle :: Resources -> L.Handle IO -> D.Handle Vk IO
resourcesToHandle resources logger =
    D.Handle {
          D.log = logger
        , D.sendRequest = H.sendRequest logger
        , D.commonEnv = commonEnv resources
        , D.getConstState = constState resources

        , D.insertUser = \u i -> modifyIORef' (usersMap resources) (M.insert u i)
        , D.getUser = \u -> readIORef (usersMap resources) >>= return . M.lookup u
        , D.specH = resourcesToVkHandler resources logger
    }



resourcesToVkHandler :: Resources -> L.Handle IO -> VkHandler IO
resourcesToVkHandler resources logger = 
    VkHandler {
          getRandomID = atomicModifyIORef' (mutState resources) getRandomID'
        , getTimestamp = fmap getTimestamp' $ readIORef (mutState resources)
        , putTimestamp = modifyIORef' (mutState resources) . putTimestamp'
        }



--initialize :: q -> Conf q -> IO (Maybe (StC q, StM q))
initialize :: L.Handle IO -> VkConfig -> IO (VkStateConst, VkStateMut)
initialize logger (VkConf methodsUrl accTok gid apiV) = do
    let
        funcName = "vk_initialize: "
        pars = [H.unit "group_id" gid] ++ defaultVkParams' accTok apiV
        initReq = H.Req H.GET (methodsUrl <> "groups.getLongPollServer") pars
        takesJson = True
    L.logDebug logger $ funcName <> "sending initialize request"
    eithInitReply <- H.sendRequest logger takesJson initReq
    initReply <- either (initRequestFail logger) return eithInitReply
    let eithParsed = parseInitResp initReply
    initData <- either (initRequestParseFail logger) return eithParsed
    initRndNum <- newStdGen
    return (VKSC {
        vkKey = _VID_key initData,
        vkServer = _VID_server initData,
        vkUrl = methodsUrl,
        vkAccessToken = accTok,
        vkGroupID = gid,
        apiVersion = apiV
      }, VKSM {
        vkTs = _VID_timestamp initData,
        vkRndGen = initRndNum
      })


initRequestFail :: L.Handle IO -> String -> IO a
initRequestFail logger err = do
    L.logFatal logger $ "Failed to get initial data"
    L.logFatal logger $ T.pack err
    Q.exitWith $ Q.ExitFailure 1

initRequestParseFail :: L.Handle IO -> String -> IO a
initRequestParseFail logger err = do
    L.logFatal logger $ "Failed to parse initial data"
    L.logFatal logger $ T.pack err
    Q.exitWith $ Q.ExitFailure 1
 

