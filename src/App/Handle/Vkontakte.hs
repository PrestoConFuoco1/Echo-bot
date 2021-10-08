module App.Handle.Vkontakte where

import qualified App.Handle as D
import qualified App.Logger as L
import Vkontakte.Types
import BotClass.ClassTypesVkInstance
import Types
import Data.IORef
import qualified Data.Map as M
import qualified HTTPRequests as H

import System.Random (StdGen, newStdGen, randomR)
import qualified Stuff as S


data Config = Config {
      configCommonEnv :: EnvironmentCommon
    , configTelegram :: VkConfig
    }

data Resources = Resources {
      commonEnv :: EnvironmentCommon
    , constState :: VkStateConst
    , mutState :: IORef VkStateMut

    , usersMap :: IORef (M.Map VkUser Int)
    }




initResources :: Config -> IO Resources
initResources (Config common vkConf) = do
    mStates <- initialize vkConf
    S.withMaybe mStates undefined $ \(sc, sm) -> do
        umap <- newIORef M.empty
        mut <- newIORef sm
        return Resources {
            commonEnv = common,
            constState = sc,
            mutState = mut,
            usersMap = umap
            }


--initialize :: q -> Conf q -> IO (Maybe (StC q, StM q))
-- i think this function can be further splitted to some lesser functions
initialize :: VkConfig -> IO (Maybe (VkStateConst, VkStateMut))
initialize (VkConf methodsUrl accTok gid apiV) = do
    let pars = [("group_id", Just . H.PIntg $ gid)] <> defaultVkParams accTok apiV
        initReq = H.Req H.GET (methodsUrl <> "groups.getLongPollServer") pars
        takesJson = True
    initReply <- {- fmap S.echo $ -} H.sendRequest takesJson initReq
    let eithParsed = initReply >>= parseInitResp
    initRndNum <- newStdGen
    return $ case eithParsed of
        Left _ -> Nothing
        Right (k, s, t) -> Just (VKSC {
            vkKey = k,
            vkServer = s,
            vkUrl = methodsUrl,
            vkAccessToken = accTok,
            vkGroupID = gid,
            apiVersion = apiV
          }, VKSM {
            vkTs = t,
            vkRndGen = initRndNum
          })


resourcesToHandle :: Resources -> L.Handle IO -> D.Handle Vk IO
resourcesToHandle resources logger =
    D.Handle {
          D.log = logger
        , D.sendRequest = H.sendRequest
        , D.commonEnv = commonEnv resources
        , D.getConstState = const (constState resources)
        , D.getMutState = const (readIORef $ mutState resources)
        , D.putMutState = const (writeIORef $ mutState resources)

        , D.insertUser = const (\u i -> modifyIORef' (usersMap resources) (M.insert u i))
        , D.getUser = const (\u -> readIORef (usersMap resources) >>= return . M.lookup u)
    }

