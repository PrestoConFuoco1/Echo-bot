{-# LANGUAGE RecordWildCards #-}

module App.Handle.Vkontakte where

import qualified App.Handle as D
import qualified App.Logger as L
import BotClass.ClassTypesVkInstance
import Data.IORef
import qualified Data.Map as M
import qualified HTTPRequests as H
import Types
import Vkontakte
import Vkontakte.Initialize

import qualified Control.Monad.Catch as C
   ( Handler(..)
   , SomeException
   , displayException
   )
import qualified Data.ByteString.Lazy.Char8 as BSL
   ( ByteString
   , unpack
   )
import qualified Data.Text as T
import GenericPretty as GP
import qualified System.Exit as Q (ExitCode(..), exitWith)
import System.Random (newStdGen)
import qualified Vkontakte.Exceptions as VkEx
import qualified Vkontakte.Send as G

data Config =
   Config
      { configCommonEnv :: EnvironmentCommon
      , configVkontakte :: VkConfig
      }

data Resources =
   Resources
      { commonEnv :: EnvironmentCommon
      , constState :: VkStateConst
      , mutState :: IORef VkStateMut
      , usersMap :: IORef (M.Map VkUser Int)
      }

initResources :: L.Handle IO -> Config -> IO Resources
initResources h (Config common vkConf) = do
   (sc, sm) <- initialize h vkConf
   umap <- newIORef M.empty
   mut <- newIORef sm
   return
      Resources
         { commonEnv = common
         , constState = sc
         , mutState = mut
         , usersMap = umap
         }

resourcesToHandle ::
      Resources -> L.Handle IO -> D.Handle Vk IO
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
              readIORef (usersMap resources) >>=
              return . M.lookup u
      , D.getUpdates = G.getUpdates logger
      , D.sendEcho = G.sendThis logger
      , D.sendHelp = G.sendThis logger
      , D.sendKeyboard = G.sendThis logger
      , D.sendRepNumMessage = G.sendThis logger
      , D.specH = resourcesToVkHandler resources logger
      }

vkErrorHandler ::
      L.Handle IO
   -> VkConfig
   -> Resources
   -> VkEx.VkException
   -> IO Resources
vkErrorHandler logger conf resources VkEx.KeyOutOfDate_GetNew =
   getNewKey logger conf resources
vkErrorHandler logger conf resources VkEx.KeyAndTsLosed_GetNew =
   getNewKeyAndTs logger conf resources

modifyKey :: T.Text -> Resources -> Resources
modifyKey key resources =
   let sc = constState resources
       sc' = sc {vkKey = key}
       resources' = resources {constState = sc'}
    in resources'

getNewKey ::
      L.Handle IO -> VkConfig -> Resources -> IO Resources
getNewKey logger config resources = do
   initData <- getLongPollServer logger config
   return $ modifyKey (_VID_key initData) resources

getNewKeyAndTs ::
      L.Handle IO -> VkConfig -> Resources -> IO Resources
getNewKeyAndTs logger config resources = do
   initData <- getLongPollServer logger config
   sm <- readIORef (mutState resources)
   let sm' = sm {vkTs = _VID_timestamp initData}
   smRef' <- newIORef sm'
   let resources' = resources {mutState = smRef'}
       resources'' =
          modifyKey (_VID_key initData) resources'
   return resources''

vkHandlers ::
      L.Handle IO
   -> VkConfig
   -> Resources
   -> [C.Handler IO Resources]
vkHandlers logger conf resources =
   [ C.Handler $ vkErrorHandler logger conf resources
   ]

defaultHandler ::
      L.Handle IO
   -> Resources
   -> C.SomeException
   -> IO Resources
defaultHandler logger _ e = do
   L.logFatal logger "unable to handle exception"
   L.logFatal logger $ T.pack $ C.displayException e
   Q.exitWith (Q.ExitFailure 1)

resourcesToVkHandler ::
      Resources -> L.Handle IO -> VkHandler IO
resourcesToVkHandler resources _ =
   VkHandler
      { getRandomID =
           atomicModifyIORef'
              (mutState resources)
              getRandomID'
      , getTimestamp =
           fmap getTimestamp' $
           readIORef (mutState resources)
      , putTimestamp =
           modifyIORef' (mutState resources) . putTimestamp'
      }

getLongPollServer ::
      L.Handle IO -> VkConfig -> IO VkInitData
getLongPollServer logger VkConf {..} = do
   let funcName = "vk_initialize: "
       pars =
          H.unit "group_id" _VC_groupID :
          defaultVkParams' _VC_accessToken _VC_apiV
       initReq =
          H.Req
             H.GET
             (_VC_vkUrl <> "groups.getLongPollServer")
             pars
       takesJson = True
   L.logDebug logger $
      funcName <> "sending initialize request"
   eithInitReply <- H.sendRequest logger takesJson initReq
   initReply <-
      either (initRequestFail logger) return eithInitReply
   let eithParsed = parseInitResp initReply
   initData <-
      either
         (initRequestParseFail logger initReply)
         return
         eithParsed
   L.logDebug logger $
      funcName <> "received initial vk api data"
   L.logDebug logger $
      funcName <> GP.defaultPrettyT initData
   return initData

initialize ::
      L.Handle IO
   -> VkConfig
   -> IO (VkStateConst, VkStateMut)
initialize logger conf@(VkConf {..}) = do
   initData <- getLongPollServer logger conf
   initRndNum <- newStdGen
   return
      ( VKSC
           { vkKey = _VID_key initData
           , vkServer = _VID_server initData
           , vkUrl = _VC_vkUrl
           , vkAccessToken = _VC_accessToken
           , vkGroupID = _VC_groupID
           , apiVersion = _VC_apiV
           }
      , VKSM
           { vkTs = _VID_timestamp initData
           , vkRndGen = initRndNum
           })

initRequestFail :: L.Handle IO -> String -> IO a
initRequestFail logger err = do
   L.logFatal logger "Failed to get initial data"
   L.logFatal logger $ T.pack err
   Q.exitWith $ Q.ExitFailure 1

initRequestParseFail ::
      L.Handle IO -> BSL.ByteString -> String -> IO a
initRequestParseFail logger s err = do
   L.logFatal logger "Failed to parse initial data"
   L.logFatal logger $ "response: " <> T.pack (BSL.unpack s)
   L.logFatal logger $ T.pack err
   Q.exitWith $ Q.ExitFailure 1
