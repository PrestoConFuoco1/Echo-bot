{-# LANGUAGE BlockArguments, DataKinds, AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables #-}
module Config where

import qualified App.Logger as L
import BotClass.ClassTypes
import BotClass.ClassTypesTeleInstance
import BotClass.ClassTypesVkInstance
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.Text as T (Text)
import qualified GenericPretty as GP
import qualified Stuff as S (withEither)
import Telegram (TlConfig(..))
import qualified Types as Y
import Vkontakte (VkConfig(..))
import qualified System.Exit as Q (ExitCode(..), exitWith, exitSuccess)
import qualified System.IO.Error as E
   ( isAlreadyInUseError
   , isDoesNotExistError
   , isPermissionError
   )
import qualified Control.Exception as E (IOException)
import qualified Control.Monad.Catch as CMC

data ConfigException =
   RequiredFieldMissing
   deriving (Show, Eq)
instance CMC.Exception ConfigException

loadConfig ::
      forall s. (BotConfigurable s)
   => L.Handle IO
   -> FilePath
   -> IO (Y.EnvironmentCommon, Conf s)
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
      L.Handle IO -> CT.Config -> IO Y.EnvironmentCommon
loadGeneral _ conf =
   CMC.handle
      (const $ pure Y.defStateGen :: CMC.SomeException -> IO Y.EnvironmentCommon) $ do
      let dg = Y.defStateGen
          f x = C.lookupDefault (x dg) conf
      confHelpMsg <- f Y.helpMsg "help_message"
      confRepQue <- f Y.repQuestion "repeat_question"
      confRepNum <- f Y.repNum "default_repeat_num"
      confTimeout <- f Y.timeout "timeout"
      confHelpCmd <- f Y.helpCommand "help_command"
      confSetRepNumCmd <-
         f Y.setRepNumCommand "set_rep_num_command"
      pure $
         Y.EnvironmentCommon {
            Y.helpMsg = confHelpMsg
            , Y.repQuestion = confRepQue
            , Y.repNum = confRepNum
            , Y.timeout = confTimeout
            , Y.helpCommand = confHelpCmd
            , Y.setRepNumCommand = confSetRepNumCmd
            }

class (BotClassTypes s) =>
      BotConfigurable s
   where
   loadSpecial ::
         L.Handle IO -> CT.Config -> IO (Conf s)
   messagerName :: T.Text

instance BotConfigurable 'Y.Telegram where
   loadSpecial logger conf =
      let teleConf = C.subconfig "telegram" conf
       in loadTeleConfig logger teleConf
   messagerName = "Y.Telegram"

instance BotConfigurable 'Y.Vkontakte where
   loadSpecial logger conf =
      let vkConf = C.subconfig "vkontakte" conf
       in loadVkConfig logger vkConf
   messagerName = "Y.Vkontakte"

tryGetConfig ::
      (BotClassTypes s)
   => L.Handle IO
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

loadTeleConfig :: L.Handle IO -> CT.Config -> IO TlConfig
loadTeleConfig _ conf = do
   initialUpdateID <- C.require conf "initial_update_id"
   botURL <- C.require conf "bot_url"
   pure $ TlConf initialUpdateID botURL

loadVkConfig :: L.Handle IO -> CT.Config -> IO VkConfig
loadVkConfig _ conf = do
   botURL <- C.require conf "bot_url"
   accTok <- C.require conf "access_token"
   groupID <- C.require conf "group_id"
   apiVersion <- C.require conf "api_version"
   pure $ VkConf botURL accTok groupID apiVersion

logKeyException :: L.Handle IO -> CT.KeyError -> IO ()
logKeyException logger = L.logError logger . f
  where
    f (CT.KeyError name) =
       "No field with name " <> name <> " found."



configHandlers :: L.Handle IO -> [CMC.Handler IO a]
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
          [ CMC.Handler (handleIOError h)
          , CMC.Handler (handleConfigError h)
          , CMC.Handler (handleConfig2Error h)
          , CMC.Handler (handleOthers h)
          ]

handleIOError :: L.Handle IO -> E.IOException -> IO ()
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

handleConfigError :: L.Handle IO -> CT.ConfigError -> IO ()
handleConfigError logger (CT.ParseError _ _) =
   L.logError logger "Failed to parse configuration file."

handleConfig2Error ::
      L.Handle IO -> ConfigException -> IO ()
handleConfig2Error logger RequiredFieldMissing =
   L.logError logger "Failed to get required field value."

handleOthers :: L.Handle IO -> CMC.SomeException -> IO ()
handleOthers logger _ =
   L.logError logger "Unknown error occured."
