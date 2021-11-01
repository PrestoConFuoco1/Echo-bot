{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module App.Handle.Vkontakte
    ( VkHandler(..)
    , defaultVkParams
    , VkStateConst(..)
    , initResources
    , resourcesToHandle
    , vkErrorHandlers
    , Resources(..)
    ) where

import qualified App.BotHandler as BotH
import qualified App.Logger as L
import BotTypesClass.VkInstance ()
import Config.Types (VkConfig(..))
import Control.Monad ((>=>))
import qualified Control.Monad.Catch as C (Handler(..))
import Data.Aeson (decode)
import Data.Aeson.Types ((.:))
import qualified Data.Aeson.Types as AeT
import qualified Data.ByteString.Lazy as BSL (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL (unpack)
import Data.IORef
import qualified Data.Map as M
import qualified Data.Text as T (Text, pack)
import qualified Environment as Env
import GHC.Generics (Generic)
import GenericPretty as GP
import qualified HTTP.Send as HS
import qualified HTTP.Types as HT
import qualified HTTP.Vkontakte as HV
import qualified Messenger as M
import qualified System.Exit as Q (ExitCode(..), exitWith)
import System.Random (StdGen, newStdGen, randomR)
import Vkontakte (VkUser(..), defaultVkParams')
import qualified Vkontakte.Exceptions as VkEx

---------- types -----------------------
instance BotH.HasBotHandler 'M.Vkontakte where
    type StateC 'M.Vkontakte = VkStateConst
    type StateM 'M.Vkontakte = VkStateMut
    type Hndl 'M.Vkontakte = VkHandler

data Resources =
    Resources
        { commonEnv :: Env.Environment
        , constState :: VkStateConst
        , mutState :: IORef VkStateMut
        , usersMap :: IORef (M.Map VkUser Int)
        }

data VkHandler m =
    VkHandler
        { getRandomID :: m Integer
        , getTimestamp :: m T.Text
        , putTimestamp :: T.Text -> m ()
        }

data VkStateConst =
    VKSC
        { vkKey :: T.Text
        , vkServer :: T.Text
        , vkUrl :: T.Text -- only for methods
        , vkAccessToken :: T.Text
        , vkGroupID :: Integer
        , apiVersion :: T.Text
        }
  deriving (Show, Eq, Generic)
  deriving anyclass (PrettyShow)

data VkStateMut =
    VKSM
        { vkTs :: T.Text -- timestamp
        , vkRndGen :: StdGen
        }
  deriving (Show)

------------------------- IO -------------------------------
initResources ::
       L.LoggerHandler IO
    -> Env.Environment
    -> VkConfig
    -> IO Resources
initResources h common vkConf = do
    (sc, sm) <- initialize h vkConf
    umap <- newIORef M.empty
    mut <- newIORef sm
    pure
        Resources
            { commonEnv = common
            , constState = sc
            , mutState = mut
            , usersMap = umap
            }

resourcesToHandle ::
       Resources
    -> L.LoggerHandler IO
    -> BotH.BotHandler 'M.Vkontakte IO
resourcesToHandle resources logger =
    BotH.BotHandler
        { BotH.log = logger
        , BotH.commonEnv = commonEnv resources
        , BotH.getConstState = constState resources
        , BotH.insertUser =
              \u i -> modifyIORef' (usersMap resources) (M.insert u i)
        , BotH.getUser =
              \u -> M.lookup u <$> readIORef (usersMap resources)
        , BotH.getUpdates = HV.getUpdates logger
        , BotH.sendEcho = HV.sendThis logger
        , BotH.sendHelp = HV.sendThis logger
        , BotH.sendKeyboard = HV.sendThis logger
        , BotH.sendRepNumMessage = HV.sendThis logger
        , BotH.specH = resourcesToVkHandler resources logger
        }

vkServerErrorHandler ::
       L.LoggerHandler IO
    -> VkConfig
    -> Resources
    -> VkEx.VkException
    -> IO Resources
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
       L.LoggerHandler IO
    -> VkConfig
    -> Resources
    -> [C.Handler IO Resources]
vkErrorHandlers logger conf resources =
    [C.Handler $ vkServerErrorHandler logger conf resources]

resourcesToVkHandler ::
       Resources -> L.LoggerHandler IO -> VkHandler IO
resourcesToVkHandler resources _ =
    VkHandler
        { getRandomID =
              atomicModifyIORef' (mutState resources) getRandomIDPure
        , getTimestamp =
              getTimestampPure <$> readIORef (mutState resources)
        , putTimestamp =
              modifyIORef' (mutState resources) . putTimestampPure
        }

getLongPollServer :: L.LoggerHandler IO -> VkConfig -> IO VkInitData
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
    L.logDebug logger $ funcName <> "sending initialize request"
    eithInitReply <- HS.sendRequest logger takesJson initReq
    initReply <- either (initRequestFail logger) pure eithInitReply
    let eithParsed = parseInitResp initReply
    initData <-
        either (initRequestParseFail logger initReply) pure eithParsed
    L.logDebug logger $ funcName <> "received initial vk api data"
    L.logDebug logger $ funcName <> GP.textPretty initData
    pure initData

initialize ::
       L.LoggerHandler IO -> VkConfig -> IO (VkStateConst, VkStateMut)
initialize logger conf@(VkConf {..}) = do
    initData <- getLongPollServer logger conf
    initRndNum <- newStdGen
    pure
        ( VKSC
              { vkKey = initKey initData
          --{ vkKey = "hahahah", -- check error handling
              , vkServer = initServer initData
              , vkUrl = vkConfigUrl
              , vkAccessToken = vkConfigAccessToken
              , vkGroupID = vkConfigGroupID
              , apiVersion = vkConfigApiVersion
              }
        , VKSM {vkTs = initTimestamp initData, vkRndGen = initRndNum})

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
data VkInitData =
    VkInitData
        { initKey :: T.Text
        , initServer :: T.Text
        , initTimestamp :: T.Text
        }
  deriving (Show, Generic)
  deriving anyclass (PrettyShow)

instance AeT.FromJSON VkInitData where
    parseJSON =
        AeT.withObject "object: key, server, ts" $ \o' -- AeT.Parser a
         -> do
            o <- o' .: "response"
            key <- o .: "key" :: AeT.Parser T.Text
            server <- o .: "server" :: AeT.Parser T.Text
            ts <- o .: "ts" :: AeT.Parser T.Text
            pure
                VkInitData
                    { initKey = key
                    , initServer = server
                    , initTimestamp = ts
                    }

parseInitResp :: BSL.ByteString -> Either String VkInitData
parseInitResp = initReplyToJSON >=> AeT.parseEither AeT.parseJSON
  where
    initReplyToJSON =
        maybe (Left "Couldn't parse getLongPollServer reply") Right .
        decode

----------------- stuff -------------------------------
defaultVkParams :: VkStateConst -> HT.ParamsList
defaultVkParams sc =
    defaultVkParams' (vkAccessToken sc) (apiVersion sc)

------------------ pure handler functions ----------------
getRandomIDPure :: VkStateMut -> (VkStateMut, Integer)
getRandomIDPure sm =
    let (rndInt32, g') =
            randomR (0 :: Integer, 2 ^ (32 - 1 :: Int)) $ vkRndGen sm
     in (sm {vkRndGen = g'}, rndInt32)

putTimestampPure :: T.Text -> VkStateMut -> VkStateMut
putTimestampPure newTs sm = sm {vkTs = newTs}

getTimestampPure :: VkStateMut -> T.Text
getTimestampPure = vkTs
