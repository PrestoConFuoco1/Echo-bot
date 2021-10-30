{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
    ( main
    ) where

import App.Handle as D
import qualified App.Handle.Telegram as T
import qualified App.Handle.Vkontakte as V
import qualified App.Logger as L
import BotTypesClass.TelegramInstance ()
import BotTypesClass.VkInstance ()
import Config.Load (BotConfigurable(..), configHandlers, loadConfig)
import Control.Monad (when)
import qualified Control.Monad.Catch as C
import qualified Data.Text as T (pack, unpack)
import qualified Environment as Env
import Execute (execute)
import Execute.BotClass
import Execute.Telegram ()
import Execute.Vkontakte ()
import qualified GenericPretty as GP
import qualified Messenger as M
import RunOptions
    ( RunOptions(..)
    , getOptsIO
    , ghciRunOpts
    , toLoggerFilter
    )
import qualified System.Exit as Q (exitSuccess)

ghciMain :: M.Messenger -> IO () -- for ghci
ghciMain m = runWithOpts (ghciRunOpts {messenger = m})

main :: IO ()
main = do
    opts <- getOptsIO
    L.logDebug L.stdHandler $ GP.textPretty opts
    runWithOpts opts

runWithOpts :: RunOptions -> IO ()
runWithOpts opts =
    case messenger opts of
        M.Telegram -> runBotWithOpts @'M.Telegram opts
        M.Vkontakte -> runBotWithOpts @'M.Vkontakte opts

runBotWithOpts ::
       forall s. (BotRun s)
    => RunOptions
    -> IO ()
runBotWithOpts opts = do
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
    L.logInfo configLogger "Successfully got bot configuration."
    when (testConfig opts) Q.exitSuccess
    L.withSelfSufficientLogger loggerConfig $ \logger ->
        mainAction @s logger gen conf `C.catch` defaultHandler logger

class (BotConfigurable s, BotClass s) => BotRun s where
    type Resources s :: *
    initResources ::
           L.LoggerHandler IO
        -> Env.Environment
        -> Conf s
        -> IO (Resources s)
    resourcesToHandler ::
           Resources s -> L.LoggerHandler IO -> BotHandler s IO
    errorHandlers ::
           L.LoggerHandler IO
        -> Conf s
        -> Resources s
        -> [C.Handler IO (Resources s)]

instance BotRun 'M.Telegram where
    type Resources 'M.Telegram = T.Resources
    initResources = T.initResources
    resourcesToHandler = T.resourcesToHandle
    errorHandlers = T.tlErrorHandlers

instance BotRun 'M.Vkontakte where
    type Resources 'M.Vkontakte = V.Resources
    initResources = V.initResources
    resourcesToHandler = V.resourcesToHandle
    errorHandlers = V.vkErrorHandlers

mainAction ::
       forall s. (BotRun s)
    => L.LoggerHandler IO
    -> Env.Environment
    -> Conf s
    -> IO ()
mainAction logger env conf = do
    resources <- initResources @s logger env conf
    _ <- forever resources action
    pure ()
  where
    action :: Resources s -> IO (Resources s)
    action res = do
        let handle = resourcesToHandler @s res logger
        (execute handle >> pure res) `C.catches`
            errorHandlers @s logger conf res

forever :: a -> (a -> IO a) -> IO a
forever res action = do
    res' <- action res
    forever res' action

defaultHandler :: L.LoggerHandler IO -> C.SomeException -> IO a
defaultHandler h e = do
    L.logFatal h "some exception raised:"
    L.logFatal h $ T.pack $ C.displayException e
    L.logFatal h "terminating..."
    C.throwM e
