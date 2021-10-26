{-# LANGUAGE BlockArguments, DataKinds, TypeApplications, AllowAmbiguousTypes, ScopedTypeVariables #-}
module Lib
   ( main
   ) where

import App.Handle as D
import qualified App.Handle.Telegram as T
import qualified App.Handle.Vkontakte as V
import qualified App.Logger as L
import BotClass.BotTeleInstance ()
import BotClass.BotVkInstance ()
import BotClass.ClassTypes (BotClassTypes(..))
import BotClass.ClassTypesTeleInstance ()
import BotClass.ClassTypesVkInstance ()
import Config (BotConfigurable(..), loadConfig, configHandlers)
import Control.Monad (when)
import qualified Control.Monad.Catch as C
import qualified Data.Configurator.Types as CT
   ( ConfigError(..)
   )
import qualified Data.Text as T (pack, unpack)
import Execute (execute)
import qualified Stuff as S (withMaybe)
import System.Environment (getArgs)
import qualified System.Exit as Q (ExitCode(..), exitWith, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Telegram (TlConfig)
import qualified Types as Y
import Vkontakte (VkConfig)
import RunOptions (RunOptions(..), getOptsIO, ghciRunOpts, toLoggerFilter)
import qualified GenericPretty as GP

ghciMain :: Y.Messenger -> IO () -- for ghci
ghciMain m =
   runWithOpts
      (ghciRunOpts {messenger = m})

main :: IO ()
main = do
    opts <- getOptsIO
    L.logDebug L.stdHandler $ GP.textPretty opts
    runWithOpts opts


runWithOpts :: RunOptions -> IO ()
runWithOpts opts =
   case messenger opts of
      Y.Telegram -> runBotWithOpts @Y.Telegram opts telegramAction
      Y.Vkontakte -> runBotWithOpts @Y.Vkontakte opts vkAction


runBotWithOpts ::
      forall s. (BotConfigurable s)
   => RunOptions
   -> (L.LoggerHandler IO -> Y.EnvironmentCommon -> Conf s -> IO ())
   -> IO ()
runBotWithOpts opts todo = do
   let configLogger =
          L.stdCondHandler $ toLoggerFilter $ loggerSettings opts
       loggerConfig =
          L.LoggerConfig
             { L.lcFilter = toLoggerFilter $ loggerSettings opts
             , L.lcPath = T.unpack $ logPath opts
             }
   (gen, conf) <-
      loadConfig @s configLogger (T.unpack $ confPath opts) `C.catches`
      configHandlers configLogger
   L.logInfo
      configLogger
      "Successfully got bot configuration."
   when (testConfig opts) Q.exitSuccess
   L.withSelfSufficientLogger loggerConfig $ \logger ->
      todo logger gen conf `C.catch` defaultHandler logger



telegramAction ::
      L.LoggerHandler IO -> Y.EnvironmentCommon -> TlConfig -> IO ()
telegramAction logger gen tlConf = do
   let tlConfig = T.Config gen tlConf
   resources <- T.initResources logger tlConfig
   _ <-
      forever resources $
      mainLoop
         tlConf
         (`T.resourcesToHandle` logger)
         D.log
         T.tlHandlers
         (execute @Y.Telegram)
   pure ()

vkAction ::
      L.LoggerHandler IO -> Y.EnvironmentCommon -> VkConfig -> IO ()
vkAction logger gen vkConf = do
   let vkConfig = V.Config gen vkConf
   resources <- V.initResources logger vkConfig
   _ <-
      forever resources $
      mainLoop
         vkConf
         (`V.resourcesToHandle` logger)
         D.log
         V.vkHandlers
         (execute @Y.Vkontakte)
   pure ()

forever :: a -> (a -> IO a) -> IO a
forever res action = do
   res' <- action res
   forever res' action

mainLoop ::
      d -- config
   -> (a -> b) -- resources to handlers
   -> (b -> L.LoggerHandler IO) -- handlers to logger
   -> (L.LoggerHandler IO -> d -> a -> [C.Handler IO a]) -- error handlers
   -> (b -> IO ()) -- handlers to execute-action
   -> a -- resources
   -> IO a
mainLoop conf resourcesToHandles toLogger errorHandlers action resources = do
   let handle = resourcesToHandles resources
       logger = toLogger handle
   (action handle >> pure resources) `C.catches`
      errorHandlers logger conf resources

defaultHandler :: L.LoggerHandler IO -> C.SomeException -> IO a
defaultHandler h e = do
   L.logFatal h "some exception raised:"
   L.logFatal h $ T.pack $ C.displayException e
   L.logFatal h "terminating..."
   C.throwM e

