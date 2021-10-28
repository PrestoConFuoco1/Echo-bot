{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lib
  ( main,
  )
where

import App.Handle as D
import qualified App.Handle.Telegram as T
import qualified App.Handle.Vkontakte as V
import qualified App.Logger as L
import BotTypesClass.TelegramInstance ()
import BotTypesClass.VkInstance ()
import Config.Load (BotConfigurable (..), configHandlers, loadConfig)
import Control.Monad (when)
import qualified Control.Monad.Catch as C
import qualified Data.Text as T (pack, unpack)
import Execute (execute)
import Execute.Telegram ()
import Execute.Vkontakte ()
import qualified GenericPretty as GP
import RunOptions (RunOptions (..), getOptsIO, ghciRunOpts, toLoggerFilter)
import qualified System.Exit as Q (exitSuccess)
import Telegram (TlConfig)
import qualified Types as Y
import qualified Messenger as M
import Vkontakte (VkConfig)

ghciMain :: M.Messenger -> IO () -- for ghci
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
    M.Telegram -> runBotWithOpts @'M.Telegram opts telegramAction
    M.Vkontakte -> runBotWithOpts @'M.Vkontakte opts vkAction

runBotWithOpts ::
  forall s.
  (BotConfigurable s) =>
  RunOptions ->
  (L.LoggerHandler IO -> Y.EnvironmentCommon -> Conf s -> IO ()) ->
  IO ()
runBotWithOpts opts todo = do
  let configLogger =
        L.stdCondHandler $ toLoggerFilter $ loggerSettings opts
      loggerConfig =
        L.LoggerConfig
          { L.lcFilter = toLoggerFilter $ loggerSettings opts,
            L.lcPath = T.unpack $ logPath opts
          }
  (gen, conf) <-
    loadConfig @s configLogger (T.unpack $ confPath opts)
      `C.catches` configHandlers configLogger
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
        (execute @'M.Telegram)
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
        (execute @'M.Vkontakte)
  pure ()

forever :: a -> (a -> IO a) -> IO a
forever res action = do
  res' <- action res
  forever res' action

mainLoop ::
  d -> -- config
  (a -> b) -> -- resources to handlers
  (b -> L.LoggerHandler IO) -> -- handlers to logger
  (L.LoggerHandler IO -> d -> a -> [C.Handler IO a]) -> -- error handlers
  (b -> IO ()) -> -- handlers to execute-action
  a -> -- resources
  IO a
mainLoop conf resourcesToHandles toLogger errorHandlers action resources = do
  let handle = resourcesToHandles resources
      logger = toLogger handle
  (action handle >> pure resources)
    `C.catches` errorHandlers logger conf resources

defaultHandler :: L.LoggerHandler IO -> C.SomeException -> IO a
defaultHandler h e = do
  L.logFatal h "some exception raised:"
  L.logFatal h $ T.pack $ C.displayException e
  L.logFatal h "terminating..."
  C.throwM e
