{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}


module App.Handle.Vkontakte where

import Data.Aeson (decode)
import Data.Aeson.Types ((.:))
import qualified Data.Aeson.Types as AeT
import qualified Data.ByteString.Lazy as BSL (ByteString)
import qualified Data.Text as T (Text, pack)
import GHC.Generics (Generic)


import qualified App.Handle as D
import qualified App.Logger as L
import BotTypesClass.VkInstance ()
import qualified Control.Monad.Catch as C
  ( Handler (..),
    SomeException,
    displayException,
  )
import qualified Data.ByteString.Lazy.Char8 as BSL
  ( ByteString,
    unpack,
  )
import Data.IORef
import qualified Data.Map as M
import GenericPretty as GP
import qualified HTTPSend as H
import qualified HTTPTypes as H
import qualified System.Exit as Q (ExitCode (..), exitWith)
import System.Random (newStdGen)
import qualified Types as Y
import Vkontakte
import qualified Vkontakte.Exceptions as VkEx
import qualified Vkontakte.Send as G

data Config = Config
  { configCommonEnv :: Y.EnvironmentCommon,
    configVkontakte :: VkConfig
  }

data Resources = Resources
  { commonEnv :: Y.EnvironmentCommon,
    constState :: VkStateConst,
    mutState :: IORef VkStateMut,
    usersMap :: IORef (M.Map VkUser Int)
  }

initResources :: L.LoggerHandler IO -> Config -> IO Resources
initResources h (Config common vkConf) = do
  (sc, sm) <- initialize h vkConf
  umap <- newIORef M.empty
  mut <- newIORef sm
  pure
    Resources
      { commonEnv = common,
        constState = sc,
        mutState = mut,
        usersMap = umap
      }

resourcesToHandle ::
  Resources -> L.LoggerHandler IO -> D.BotHandler 'Y.Vkontakte IO
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
          M.lookup u
            <$> readIORef (usersMap resources),
      D.getUpdates = G.getUpdates logger,
      D.sendEcho = G.sendThis logger,
      D.sendHelp = G.sendThis logger,
      D.sendKeyboard = G.sendThis logger,
      D.sendRepNumMessage = G.sendThis logger,
      D.specH = resourcesToVkHandler resources logger
    }

vkErrorHandler ::
  L.LoggerHandler IO ->
  VkConfig ->
  Resources ->
  VkEx.VkException ->
  IO Resources
vkErrorHandler logger conf resources VkEx.KeyOutOfDateGetNew =
  getNewKey logger conf resources
vkErrorHandler logger conf resources VkEx.KeyAndTsLosedGetNew =
  getNewKeyAndTs logger conf resources

modifyKey :: T.Text -> Resources -> Resources
modifyKey key resources =
  let sc = constState resources
      sc' = sc {vkKey = key}
   in resources {constState = sc'}

getNewKey ::
  L.LoggerHandler IO -> VkConfig -> Resources -> IO Resources
getNewKey logger config resources = do
  initData <- getLongPollServer logger config
  pure $ modifyKey (initKey initData) resources

getNewKeyAndTs ::
  L.LoggerHandler IO -> VkConfig -> Resources -> IO Resources
getNewKeyAndTs logger config resources = do
  initData <- getLongPollServer logger config
  sm <- readIORef (mutState resources)
  let sm' = sm {vkTs = initTimestamp initData}
  smRef' <- newIORef sm'
  let resources' = resources {mutState = smRef'}
  pure $ modifyKey (initKey initData) resources'

vkHandlers ::
  L.LoggerHandler IO ->
  VkConfig ->
  Resources ->
  [C.Handler IO Resources]
vkHandlers logger conf resources =
  [ C.Handler $ vkErrorHandler logger conf resources
  ]

defaultHandler ::
  L.LoggerHandler IO ->
  Resources ->
  C.SomeException ->
  IO Resources
defaultHandler logger _ e = do
  L.logFatal logger "unable to handle exception"
  L.logFatal logger $ T.pack $ C.displayException e
  Q.exitWith (Q.ExitFailure 1)

resourcesToVkHandler ::
  Resources -> L.LoggerHandler IO -> VkHandler IO
resourcesToVkHandler resources _ =
  VkHandler
    { getRandomID =
        atomicModifyIORef'
          (mutState resources)
          getRandomIDPure,
      getTimestamp =
        getTimestampPure
          <$> readIORef (mutState resources),
      putTimestamp =
        modifyIORef' (mutState resources) . putTimestampPure
    }

getLongPollServer ::
  L.LoggerHandler IO -> VkConfig -> IO VkInitData
getLongPollServer logger VkConf {..} = do
  let funcName = "vk_initialize: "
      pars =
        H.unit "group_id" configGroupID :
        defaultVkParams' configAccessToken configApiVersion
      initReq =
        H.Req
          H.GET
          (configUrl <> "groups.getLongPollServer")
          pars
      takesJson = True
  L.logDebug logger $
    funcName <> "sending initialize request"
  eithInitReply <- H.sendRequest logger takesJson initReq
  initReply <-
    either (initRequestFail logger) pure eithInitReply
  let eithParsed = parseInitResp initReply
  initData <-
    either
      (initRequestParseFail logger initReply)
      pure
      eithParsed
  L.logDebug logger $
    funcName <> "received initial vk api data"
  L.logDebug logger $
    funcName <> GP.defaultPrettyT initData
  pure initData

initialize ::
  L.LoggerHandler IO ->
  VkConfig ->
  IO (VkStateConst, VkStateMut)
initialize logger conf@(VkConf {..}) = do
  initData <- getLongPollServer logger conf
  initRndNum <- newStdGen
  pure
    ( VKSC
        { vkKey = initKey initData,
          vkServer = initServer initData,
          vkUrl = configUrl,
          vkAccessToken = configAccessToken,
          vkGroupID = configGroupID,
          apiVersion = configApiVersion
        },
      VKSM
        { vkTs = initTimestamp initData,
          vkRndGen = initRndNum
        }
    )

initRequestFail :: L.LoggerHandler IO -> String -> IO a
initRequestFail logger err = do
  L.logFatal logger "Failed to get initial data"
  L.logFatal logger $ T.pack err
  Q.exitWith $ Q.ExitFailure 1

initRequestParseFail ::
  L.LoggerHandler IO -> BSL.ByteString -> String -> IO a
initRequestParseFail logger s err = do
  L.logFatal logger "Failed to parse initial data"
  L.logFatal logger $ "response: " <> T.pack (BSL.unpack s)
  L.logFatal logger $ T.pack err
  Q.exitWith $ Q.ExitFailure 1


-----------------------------------------
data VkInitData = VkInitData
  { initKey :: T.Text,
    initServer :: T.Text,
    initTimestamp :: T.Text
  }
  deriving (Show, Generic)
  deriving anyclass (PrettyShow)

parseInitResp :: BSL.ByteString -> Either String VkInitData
parseInitResp = eithParsed
  where
    parseInitRep =
      AeT.withObject "object: key, server, ts" $ \o' -> -- AeT.Parser a
        do
          o <- o' .: "response"
          key <- o .: "key" :: AeT.Parser T.Text
          server <- o .: "server" :: AeT.Parser T.Text
          ts <- o .: "ts" :: AeT.Parser T.Text
          pure
            VkInitData
              { initKey = key,
                initServer = server,
                initTimestamp = ts
              }
    initReplyToJSON =
      maybe
        (Left "Couldn't parse getLongPollServer reply")
        Right
        . decode
    eithParsed x =
      initReplyToJSON x >>= AeT.parseEither parseInitRep
