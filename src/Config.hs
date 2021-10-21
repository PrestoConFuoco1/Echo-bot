module Config where

import qualified App.Logger as L
import BotClass.ClassTypes
import BotClass.ClassTypesTeleInstance
import BotClass.ClassTypesVkInstance
import qualified Control.Exception as E
   ( Exception(..)
   , SomeException
   , SomeException
   , handle
   , throw
   , try
   )
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.Text as T (Text)
import qualified GenericPretty as GP
import qualified Stuff as S (withEither)
import Telegram (TlConfig(..))
import Types
import Vkontakte (VkConfig(..))

data ConfigException =
   RequiredFieldMissing
   deriving (Show, Eq)

instance E.Exception ConfigException

-----------------------------------------------------
loadConfig ::
      (BotConfigurable s)
   => s
   -> L.Handle IO
   -> FilePath
   -> IO (EnvironmentCommon, Conf s)
loadConfig s logger path = do
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
      tryGetConfig s logger (messagerName s) $
      loadSpecial s logger conf
   L.logDebug
      logger
      "Loaded messager-specific configuration:"
   L.logDebug logger $ GP.textPretty stSpec
   return (stGen, stSpec)

loadGeneral ::
      L.Handle IO -> CT.Config -> IO EnvironmentCommon
loadGeneral _ conf =
   E.handle
      (const $ return defStateGen :: E.SomeException -> IO EnvironmentCommon) $ do
      let dg = defStateGen
          f x = C.lookupDefault (x dg) conf
      confHelpMsg <- f helpMsg "help_message"
      confRepQue <- f repQuestion "repeat_question"
      confRepNum <- f repNum "default_repeat_num"
      confTimeout <- f timeout "timeout"
      confHelpCmd <- f helpCommand "help_command"
      confSetRepNumCmd <-
         f setRepNumCommand "set_rep_num_command"
      return $
         EnvironmentCommon
            confHelpMsg
            confRepQue
            confRepNum
            confTimeout
            confHelpCmd
            confSetRepNumCmd

class (BotClassTypes s) =>
      BotConfigurable s
   where
   loadSpecial ::
         s -> L.Handle IO -> CT.Config -> IO (Conf s)
   messagerName :: s -> T.Text

instance BotConfigurable Tele where
   loadSpecial _ logger conf =
      let teleConf = C.subconfig "telegram" conf
       in loadTeleConfig logger teleConf
   messagerName _ = "Telegram"

instance BotConfigurable Vk where
   loadSpecial _ logger conf =
      let vkConf = C.subconfig "vkontakte" conf
       in loadVkConfig logger vkConf
   messagerName _ = "Vkontakte"

tryGetConfig ::
      (BotClassTypes s)
   => s
   -> L.Handle IO
   -> T.Text
   -> IO (Conf s)
   -> IO (Conf s)
tryGetConfig _ logger messager atry = do
   L.logDebug logger $
      "Trying to get " <> messager <> " bot configuration"
   eithStMsgError <- E.try atry -- :: IO (Either CT.KeyError a)
   S.withEither
      eithStMsgError
      (\e -> do
          L.logError logger $
             messager <> ": configuration error occured:"
          logKeyException logger e
          E.throw RequiredFieldMissing)
      (\c -> do
          L.logDebug logger $
             "Ok, " <> messager <> " bot config loaded."
          return c)

loadTeleConfig :: L.Handle IO -> CT.Config -> IO TlConfig
loadTeleConfig _ conf = do
   initialUpdateID <- C.require conf "initial_update_id"
   botURL <- C.require conf "bot_url"
   return $ TlConf initialUpdateID botURL

loadVkConfig :: L.Handle IO -> CT.Config -> IO VkConfig
loadVkConfig _ conf = do
   botURL <- C.require conf "bot_url"
   accTok <- C.require conf "access_token"
   groupID <- C.require conf "group_id"
   apiVersion <- C.require conf "api_version"
   return $ VkConf botURL accTok groupID apiVersion

logKeyException :: L.Handle IO -> CT.KeyError -> IO ()
logKeyException logger = L.logError logger . f
  where
    f (CT.KeyError name) =
       "No field with name " <> name <> " found." {-T.fromStrict-}
