{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Config where

import qualified Data.Text.Lazy as TL (Text, fromStrict, pack)
import qualified Data.Text as T (Text, pack)
import GHC.Generics (Generic)
import qualified GenericPretty as GP
import qualified Stuff as S
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Control.Exception as E (catch, SomeException, Exception (..),
                                         try, throw, SomeException)
import qualified App.Logger as L

import TeleTypes (TlConfig (..), defaultTlConfig)
import VkTypes  (VkConfig (..), defaultVkConfig)

import BotTypes (EnvironmentCommon (..), helpMsg, repQuestion, repNum, timeout,
                 helpCommand, setRepNumCommand, defStateGen)

-----------------------------------------------------

data BotConfig = TlC TlConfig | VkC VkConfig deriving (Show, Generic)

instance GP.PrettyShow BotConfig where
    prettyShow (TlC tl) = GP.genericPrettyShow GP.defaultOptionsL {
        GP.consModifier = id
        } $ tl
    prettyShow (VkC vk) = GP.genericPrettyShow GP.defaultOptionsL {
        GP.consModifier = id
        } $ vk




data ConfigException = RequiredFieldMissing
    deriving (Show, Eq)

instance E.Exception ConfigException

-----------------------------------------------------

loadConfig :: L.Handle IO -> FilePath -> IO (EnvironmentCommon, BotConfig)
loadConfig logger path = do
    conf <- C.load [CT.Required path]

    let genConf = C.subconfig "general" conf
    L.logDebug logger "Loading general messager-independent configuration."
    L.logDebug logger "This step cannot fail as this way defaults will be used"
    stGen <- loadGeneral logger genConf
    L.logDebug logger "Loaded general configuration:"
    L.logDebug logger $ T.pack $ GP.defaultPretty stGen
    stSpec <- loadSpecial logger conf
    L.logDebug logger "Loaded messager-specific configuration:"
    L.logDebug logger $ T.pack $ GP.defaultPretty stSpec
    return (stGen, stSpec)


loadGeneral :: L.Handle IO -> CT.Config -> IO EnvironmentCommon
loadGeneral logger conf = (flip E.catch $
        (const $ return defStateGen ::
            E.SomeException -> IO EnvironmentCommon)) $ do
    let dg = defStateGen
        f x s = C.lookupDefault (x dg) conf s
    confHelpMsg <- f helpMsg "help_message"
    confRepQue  <- f repQuestion "repeat_question"
    confRepNum  <- f repNum "default_repeat_num"
    confTimeout <- f timeout "timeout"
    confHelpCmd <- f helpCommand "help_command"
    confSetRepNumCmd <- f setRepNumCommand "set_rep_num_command"
    return $ EnvironmentCommon confHelpMsg confRepQue confRepNum confTimeout confHelpCmd confSetRepNumCmd

loadSpecial :: L.Handle IO -> CT.Config -> IO BotConfig
loadSpecial logger conf = foldr (tryGetConfig logger) (E.throw RequiredFieldMissing) whatToDo
  where vkConf = C.subconfig "vkontakte" conf
        teleConf = C.subconfig "telegram" conf

        whatToDo = [(fmap TlC $ loadTeleConfig logger teleConf, "Telegram"),
                    (fmap VkC $ loadVkConfig logger vkConf, "Vkontakte")]

tryGetConfig :: L.Handle IO -> (IO BotConfig, T.Text) -> IO BotConfig -> IO BotConfig
tryGetConfig logger (atry, messager) acc = do
    L.logDebug logger $ "Trying to get " <> messager <> " bot configuration"
    eithStMsger <- E.try atry :: IO (Either CT.KeyError BotConfig)
    ($ eithStMsger) $ either
        (\e -> do
            L.logError logger $ messager <> ": configuration error occured:"
            logKeyException logger e
            acc) 
        (\c -> do
            L.logDebug logger $ "Ok, " <> messager <> " bot config loaded." 
            return c)
   

loadTeleConfig :: L.Handle IO -> CT.Config -> IO TlConfig
loadTeleConfig logger conf = do
    initialUpdateID <- C.lookupDefault (_TC_updID defaultTlConfig) conf "initial_update_id"
    botURL <- C.require conf "bot_url"
    return $ TlConf initialUpdateID botURL

loadVkConfig :: L.Handle IO -> CT.Config -> IO VkConfig
loadVkConfig logger conf = do
    botURL <- C.require conf "bot_url"
    accTok <- C.require conf "access_token"
    groupID <- C.require conf "group_id"
    apiVersion <- C.require conf "api_version"
    return $ VkConf botURL accTok groupID apiVersion


logKeyException :: L.Handle IO -> CT.KeyError -> IO ()
logKeyException logger = L.logError L.simpleLog . f
  where f (CT.KeyError name) = "No field with name " <> {-TL.fromStrict-} name <> " found."
