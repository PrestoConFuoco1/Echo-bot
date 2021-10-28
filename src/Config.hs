{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Config where

import qualified App.Logger as L
import qualified Control.Exception as E (IOException)
import qualified Control.Monad.Catch as CMC
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.Text as T (Text)
import qualified GenericPretty as GP
import qualified Stuff as S (withEither)
import qualified System.Exit as Q (ExitCode (..), exitWith)
import qualified System.IO.Error as E
  ( isAlreadyInUseError,
    isDoesNotExistError,
    isPermissionError,
  )
import Telegram (TlConfig (..))
import qualified Types as Y
import Vkontakte (VkConfig (..))

data ConfigException
  = RequiredFieldMissing
  deriving (Show, Eq)

instance CMC.Exception ConfigException

loadConfig ::
  forall s.
  (BotConfigurable s) =>
  L.LoggerHandler IO ->
  FilePath ->
  IO (Y.EnvironmentCommon, Conf s)
loadConfig logger path = do
  conf <- C.load [CT.Required path]
  let genConf = C.subconfig "general" conf
  L.logDebug
    logger
    "Loading general messager-independent configuration."
  L.logDebug
    logger
    "This step cannot fail as this way defaults will be used"
  stGen <- loadGeneral logger genConf
  L.logDebug logger "Loaded general configuration:"
  L.logDebug logger $ GP.textPretty stGen
  stSpec <-
    tryGetConfig @s logger (messagerName @s) $
      loadSpecial @s logger conf
  L.logDebug
    logger
    "Loaded messager-specific configuration:"
  L.logDebug logger $ GP.textPretty stSpec
  pure (stGen, stSpec)

loadGeneral ::
  L.LoggerHandler IO -> CT.Config -> IO Y.EnvironmentCommon
loadGeneral _ conf =
  CMC.handle
    (const $ pure Y.defStateGen :: CMC.SomeException -> IO Y.EnvironmentCommon)
    $ do
      let dg = Y.defStateGen
          withDefault :: (CT.Configured a) => (Y.EnvironmentCommon -> a) -> CT.Name -> IO a
          withDefault f = C.lookupDefault (f dg) conf
      confHelpMsg <- withDefault Y.getHelpMessage "help_message"
      confRepQue <- withDefault Y.getRepeatQuestion "repeat_question"
      confRepNum <- withDefault Y.repNum "default_repeat_num"
      confTimeout <- withDefault Y.timeout "timeout"
      confHelpCmd <- withDefault Y.getHelpCommand "help_command"
      confSetRepNumCmd <-
        withDefault Y.getSetRepNumCommand "set_rep_num_command"
      let cmds =
            Y.EnvCommands
              { Y.helpCommand = confHelpCmd,
                Y.setRepNumCommand = confSetRepNumCmd
              }
          msgs =
            Y.EnvMessages
              { Y.helpMsg = confHelpMsg,
                Y.repQuestion = confRepQue
              }
      pure $
        Y.EnvironmentCommon
          { Y.envCommands = cmds,
            Y.envMessages = msgs,
            Y.repNum = confRepNum,
            Y.timeout = confTimeout
          }

class (GP.PrettyShow (Conf s)) =>
  BotConfigurable (s :: Y.Messenger)
  where
  type Conf s :: *
  loadSpecial ::
    L.LoggerHandler IO -> CT.Config -> IO (Conf s)
  messagerName :: T.Text

instance BotConfigurable 'Y.Telegram where
  type Conf 'Y.Telegram = TlConfig
  loadSpecial logger conf =
    let teleConf = C.subconfig "telegram" conf
     in loadTeleConfig logger teleConf
  messagerName = "Telegram"

instance BotConfigurable 'Y.Vkontakte where
  type Conf 'Y.Vkontakte = VkConfig
  loadSpecial logger conf =
    let vkConf = C.subconfig "vkontakte" conf
     in loadVkConfig logger vkConf
  messagerName = "Vkontakte"

tryGetConfig ::
  (BotConfigurable s) =>
  L.LoggerHandler IO ->
  T.Text ->
  IO (Conf s) ->
  IO (Conf s)
tryGetConfig logger messager atry = do
  L.logDebug logger $
    "Trying to get " <> messager <> " bot configuration"
  eithStMsgError <- CMC.try atry -- :: IO (Either CT.KeyError a)
  S.withEither
    eithStMsgError
    ( \e -> do
        L.logError logger $
          messager <> ": configuration error occured:"
        logKeyException logger e
        CMC.throwM RequiredFieldMissing
    )
    ( \c -> do
        L.logDebug logger $
          "Ok, " <> messager <> " bot config loaded."
        pure c
    )

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
    f (CT.KeyError name) =
      "No field with name " <> name <> " found."

configHandlers :: L.LoggerHandler IO -> [CMC.Handler IO a]
configHandlers h =
  let f (CMC.Handler g) =
        CMC.Handler
          \e -> do
            g e
            L.logFatal
              h
              "Failed to get required data from configuration files, terminating..."
            Q.exitWith (Q.ExitFailure 1)
   in map
        f
        [ CMC.Handler (handleIOError h),
          CMC.Handler (handleConfigError h),
          CMC.Handler (handleConfig2Error h),
          CMC.Handler (handleOthers h)
        ]

handleIOError :: L.LoggerHandler IO -> E.IOException -> IO ()
handleIOError logger exc
  | E.isDoesNotExistError exc =
    L.logError logger "File does not exist."
  | E.isPermissionError exc =
    L.logError
      logger
      "Not enough permissions to open file."
  | E.isAlreadyInUseError exc =
    L.logError logger "File is already in use."
  | otherwise = L.logError logger "Unknown error occured"

handleConfigError :: L.LoggerHandler IO -> CT.ConfigError -> IO ()
handleConfigError logger (CT.ParseError _ _) =
  L.logError logger "Failed to parse configuration file."

handleConfig2Error ::
  L.LoggerHandler IO -> ConfigException -> IO ()
handleConfig2Error logger RequiredFieldMissing =
  L.logError logger "Failed to get required field value."

handleOthers :: L.LoggerHandler IO -> CMC.SomeException -> IO ()
handleOthers logger _ =
  L.logError logger "Unknown error occured."
