{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Config.Load
    ( loadConfig
    , configHandlers
    , BotConfigurable(..)
    ) where

import qualified App.Logger as L
import Config.Types (TlConfig(..), VkConfig(..))
import qualified Control.Exception as E (IOException)
import qualified Control.Monad.Catch as CMC
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.Text as T (Text)
import qualified Environment as Env
import qualified GenericPretty as GP
import qualified Messenger as M
import qualified Stuff as S (withEither)
import qualified System.Exit as Q (ExitCode(..), exitWith)
import qualified System.IO.Error as E
    ( isAlreadyInUseError
    , isDoesNotExistError
    , isPermissionError
    )

data ConfigException =
    RequiredFieldMissing
  deriving (Show, Eq)

instance CMC.Exception ConfigException

loadConfig ::
       forall s. (BotConfigurable s)
    => L.LoggerHandler IO
    -> FilePath
    -> IO (Env.Environment, Conf s)
loadConfig logger path = do
    conf <- C.load [CT.Required path]
    let genConf = C.subconfig "general" conf
    L.logDebug
        logger
        "Loading general messager-independent configuration."
    stGen <- loadGeneral logger genConf
    L.logDebug logger "Loaded general configuration:"
    L.logDebug logger $ GP.textPretty stGen
    stSpec <-
        tryGetConfig @s logger (messagerName @s) $
        loadSpecial @s logger conf
    L.logDebug logger "Loaded messager-specific configuration:"
    L.logDebug logger $ GP.textPretty stSpec
    pure (stGen, stSpec)

withDefault ::
       (CT.Configured a)
    => CT.Config
    -> (Env.Environment -> a)
    -> CT.Name
    -> IO a
withDefault conf getDefault =
    C.lookupDefault (getDefault Env.defStateGen) conf


loadGeneral :: L.LoggerHandler IO -> CT.Config -> IO Env.Environment
loadGeneral _ conf = do
    confHelpMsg <-
        withDefault conf Env.getHelpMessage "help_message"
    confRepQue <-
        withDefault conf Env.getRepeatQuestion "repeat_question"
    confRepNum <- withDefault conf Env.getRepNum "default_repeat_num"
    confTimeout <- withDefault conf Env.getTimeout "timeout"
    confHelpCmd <-
        withDefault conf Env.getHelpCommand "help_command"
    confSetRepNumCmd <-
        withDefault
            conf
            Env.getSetRepNumCommand
            "set_rep_num_command"
    let cmds = Env.newEnvCommands confHelpCmd confSetRepNumCmd
        msgs = Env.newEnvMessages confHelpMsg confRepQue
    pure $ Env.newEnvironment confRepNum confTimeout msgs cmds

class (GP.PrettyShow (Conf s)) =>
      BotConfigurable (s :: M.Messenger)
    where
    type Conf s :: *
    loadSpecial :: L.LoggerHandler IO -> CT.Config -> IO (Conf s)
    messagerName :: T.Text

instance BotConfigurable 'M.Telegram where
    type Conf 'M.Telegram = TlConfig
    loadSpecial logger conf =
        let teleConf = C.subconfig "telegram" conf
         in loadTeleConfig logger teleConf
    messagerName = "Telegram"

instance BotConfigurable 'M.Vkontakte where
    type Conf 'M.Vkontakte = VkConfig
    loadSpecial logger conf =
        let vkConf = C.subconfig "vkontakte" conf
         in loadVkConfig logger vkConf
    messagerName = "Vkontakte"

tryGetConfig ::
       (BotConfigurable s)
    => L.LoggerHandler IO
    -> T.Text
    -> IO (Conf s)
    -> IO (Conf s)
tryGetConfig logger messager atry = do
    L.logDebug logger $
        "Trying to get " <> messager <> " bot configuration"
    eithStMsgError <- CMC.try atry -- :: IO (Either CT.KeyError a)
    S.withEither
        eithStMsgError
        (\e -> do
             L.logError logger $
                 messager <> ": configuration error occured:"
             logKeyException logger e
             CMC.throwM RequiredFieldMissing)
        (\c -> do
             L.logDebug logger $
                 "Ok, " <> messager <> " bot config loaded."
             pure c)

loadTeleConfig :: L.LoggerHandler IO -> CT.Config -> IO TlConfig
loadTeleConfig _ conf = do
    initialUpdateID <- C.require conf "initial_update_id"
    botURL <- C.require conf "bot_url"
    pure $ TlConf initialUpdateID botURL

loadVkConfig :: L.LoggerHandler IO -> CT.Config -> IO VkConfig
loadVkConfig _ conf = do
    botURL <- C.require conf "bot_url"
    accTok <- C.require conf "access_token"
    groupID <- C.require conf "group_id"
    apiVersion <- C.require conf "api_version"
    pure $ VkConf botURL accTok groupID apiVersion

logKeyException :: L.LoggerHandler IO -> CT.KeyError -> IO ()
logKeyException logger = L.logError logger . f
  where
    f (CT.KeyError name) = "No field with name " <> name <> " found."

configHandlers :: L.LoggerHandler IO -> [CMC.Handler IO a]
configHandlers logger =
    map (terminateAfterHandler logger)
        [ CMC.Handler (handleIOError logger)
        , CMC.Handler (handleConfigError logger)
        , CMC.Handler (handleConfig2Error logger)
        , CMC.Handler (handleOthers logger)
        ]

terminateAfterHandler ::
       L.LoggerHandler IO -> CMC.Handler IO () -> CMC.Handler IO a
terminateAfterHandler logger (CMC.Handler g) =
    let configFailMsg = 
            "Failed to get required data from configuration files, terminating..."
     in CMC.Handler $ \e -> do
        g e
        L.logFatal logger configFailMsg
        Q.exitWith (Q.ExitFailure 1)

handleIOError :: L.LoggerHandler IO -> E.IOException -> IO ()
handleIOError logger exc
    | E.isDoesNotExistError exc =
        L.logError logger "File does not exist."
    | E.isPermissionError exc =
        L.logError logger "Not enough permissions to open file."
    | E.isAlreadyInUseError exc =
        L.logError logger "File is already in use."
    | otherwise = L.logError logger "Unknown error occured"

handleConfigError :: L.LoggerHandler IO -> CT.ConfigError -> IO ()
handleConfigError logger (CT.ParseError _ _) =
    L.logError logger "Failed to parse configuration file."

handleConfig2Error :: L.LoggerHandler IO -> ConfigException -> IO ()
handleConfig2Error logger RequiredFieldMissing =
    L.logError logger "Failed to get required field value."

handleOthers :: L.LoggerHandler IO -> CMC.SomeException -> IO ()
handleOthers logger _ = L.logError logger "Unknown error occured."
