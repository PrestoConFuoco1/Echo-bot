{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module App.Handle.Vkontakte (
    VkHandler(..), defaultVkParams, VkStateConst (..), initResources, resourcesToHandle, vkErrorHandlers, Resources (..)
) where

import qualified App.Handle as D
import qualified App.Logger as L
import BotTypesClass.VkInstance ()
import qualified Control.Monad.Catch as C (Handler (..))
import Data.Aeson (decode)
import Data.Aeson.Types ((.:))
import qualified Data.Aeson.Types as AeT
import qualified Data.ByteString.Lazy as BSL (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
  ( unpack,
  )
import Data.IORef
import qualified Data.Map as M
import qualified Data.Text as T (Text, pack)
import GHC.Generics (Generic)
import GenericPretty as GP
import qualified HTTP.Send as HS
import qualified HTTP.Types as HT
import qualified System.Exit as Q (ExitCode (..), exitWith)
import System.Random (newStdGen)
import qualified Messenger as M
import Vkontakte (VkUser(..), defaultVkParams')
import qualified Vkontakte.Exceptions as VkEx
import qualified HTTP.Vkontakte as HV
import Config.Types (VkConfig(..))
import qualified Environment as Env
import System.Random (randomR, StdGen)



---------- types -----------------------
instance D.HasBotHandler 'M.Vkontakte where
    type StateC 'M.Vkontakte = VkStateConst
    type StateM 'M.Vkontakte = VkStateMut
    type Hndl 'M.Vkontakte = VkHandler
{-
data Config = Config
  { configCommonEnv :: Env.EnvironmentCommon,
    configVkontakte :: VkConfig
  }
-}
data Resources = Resources
  { commonEnv :: Env.EnvironmentCommon,
    constState :: VkStateConst,
    mutState :: IORef VkStateMut,
    usersMap :: IORef (M.Map VkUser Int)
  }

data VkHandler m = VkHandler
  { getRandomID :: m Integer,
    getTimestamp :: m T.Text,
    putTimestamp :: T.Text -> m ()
  }

data VkStateConst = VKSC
  { vkKey :: T.Text,
    vkServer :: T.Text,
    vkUrl :: T.Text, -- only for methods
    vkAccessToken :: T.Text,
    vkGroupID :: Integer,
    apiVersion :: T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyShow)

data VkStateMut = VKSM
  { vkTs :: T.Text, -- timestamp
    vkRndGen :: StdGen
  }
  deriving stock (Show)

------------------------- IO -------------------------------
initResources :: L.LoggerHandler IO -> Env.EnvironmentCommon -> VkConfig -> IO Resources
initResources h common vkConf = do
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
  Resources -> L.LoggerHandler IO -> D.BotHandler 'M.Vkontakte IO
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
      D.getUpdates = HV.getUpdates logger,
      D.sendEcho = HV.sendThis logger,
      D.sendHelp = HV.sendThis logger,
      D.sendKeyboard = HV.sendThis logger,
      D.sendRepNumMessage = HV.sendThis logger,
      D.specH = resourcesToVkHandler resources logger
    }

vkServerErrorHandler ::
  L.LoggerHandler IO ->
  VkConfig ->
  Resources ->
  VkEx.VkException ->
  IO Resources
vkServerErrorHandler logger conf resources VkEx.KeyOutOfDateGetNew =
  getNewKey logger conf resources
vkServerErrorHandler logger conf resources VkEx.KeyAndTsLosedGetNew =
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

vkErrorHandlers ::
  L.LoggerHandler IO ->
  VkConfig ->
  Resources ->
  [C.Handler IO Resources]
vkErrorHandlers logger conf resources =
  [ C.Handler $ vkServerErrorHandler logger conf resources
  ]

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
        HT.unit "group_id" vkConfigGroupID :
        defaultVkParams' vkConfigAccessToken vkConfigApiVersion
      initReq =
        HT.Req
          HT.GET
          (vkConfigUrl <> "groups.getLongPollServer")
          pars
      takesJson = True
  L.logDebug logger $
    funcName <> "sending initialize request"
  eithInitReply <- HS.sendRequest logger takesJson initReq
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
        --{ vkKey = initKey initData,
        { vkKey = "hahahah",
          vkServer = initServer initData,
          vkUrl = vkConfigUrl,
          vkAccessToken = vkConfigAccessToken,
          vkGroupID = vkConfigGroupID,
          apiVersion = vkConfigApiVersion
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


----------------- stuff -------------------------------
defaultVkParams :: VkStateConst -> HT.ParamsList
defaultVkParams sc =
  defaultVkParams' (vkAccessToken sc) (apiVersion sc)

------------------ pure handler functions ----------------
getRandomIDPure :: VkStateMut -> (VkStateMut, Integer)
getRandomIDPure sm =
  let (rndInt32, g') =
        randomR (0 :: Integer, 2 ^ (32 - 1 :: Int)) $
          vkRndGen sm
   in (sm {vkRndGen = g'}, rndInt32)

putTimestampPure :: T.Text -> VkStateMut -> VkStateMut
putTimestampPure newTs sm = sm {vkTs = newTs}

getTimestampPure :: VkStateMut -> T.Text
getTimestampPure = vkTs



