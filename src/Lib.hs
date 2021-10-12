module Lib
    ( someFunc
    ) where


import Config
import qualified Control.Exception as E
    (catches, Handler (..), SomeException, IOException)
import qualified Control.Monad.Catch as C (catches, Handler (..))
import qualified System.IO.Error as E
    (isDoesNotExistError, isPermissionError, isAlreadyInUseError)
import qualified Data.Configurator.Types as CT (ConfigError (..))
import qualified System.Exit as Q (ExitCode (..), exitWith)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import qualified App.Logger as L
import Control.Monad (when, forever)
import Execute

import qualified App.Handle.Telegram as T
import qualified App.Handle.Vkontakte as V
import BotClass.ClassTypes
import Types
import BotClass.BotVkInstance
import BotClass.BotTeleInstance
import Telegram.Types (Tele(..))
import Vkontakte.Types (Vk(..))
import Data.IORef
import App.Handle as D
import qualified Stuff as S (withMaybe)
import Data.List (isPrefixOf)

data Messager = Vkontakte | Telegram | None
data RunOptions = RunOptions {
    testConfig :: Bool
    , loggerSettings :: L.Priority -> Bool
    , messager :: Messager
    }
defaultRunOpts = RunOptions {
    testConfig = False
    , loggerSettings = const True
    , messager = None }

qomeFunc :: Messager -> IO ()
qomeFunc m = runWithConf (defaultRunOpts {messager = m}) "src/bot.conf"

someFunc :: IO ()
someFunc = do
    args <- getArgs
    case args of
        [] -> hPutStrLn stderr "Expected path to configuration file." >>
                Q.exitWith (Q.ExitFailure 1)
        (x:xs) -> runWithConf (getOpts xs) x

getOpts :: [String] -> RunOptions
getOpts = foldr f defaultRunOpts
  where f "--test-config" acc = acc { testConfig = True }
        f "-vk" acc = acc { messager = Vkontakte }
        f "-tl" acc = acc { messager = Telegram }
        f str acc
            | "-l" `isPrefixOf` str =
                let newSettings accFunc f = \x -> f x && accFunc x
                    accF = loggerSettings acc
                in  S.withMaybe (getLoggerSettings $ drop 2 str)
                        acc (\x -> acc { loggerSettings = newSettings accF x })
        f _ acc = acc

getLoggerSettings = undefined

runWithConf :: RunOptions -> FilePath -> IO ()
runWithConf opts path =
    case messager opts of
        Telegram -> runWithConf' Tele opts path $ func tlAction
        --Vkontakte -> runWithConf' Vk opts path (\x y -> vkAction x y >> return ())
        Vkontakte -> runWithConf' Vk opts path $ func vkAction
        None -> L.logFatal logger "No messager parameter supplied, terminating..." >>
                L.logInfo  logger "Use -tl for Telegram and -vk for Vkontakte"
  where
        func a = \x y -> a x y >> return ()
        logger = L.simpleLog
        tlAction gen tlConf = do
            let tlConfig = T.Config gen tlConf
            resources <- T.initResources logger tlConfig
            let handle = T.resourcesToHandle resources logger
            --forever (execute handle Tele)
            forever' resources $ mainLoop
                tlConf
                (flip T.resourcesToHandle logger)
                D.log
                T.tlHandlers
                (flip execute Tele)
        vkAction gen vkConf = do
            let vkConfig = V.Config gen vkConf
            resources <- V.initResources logger vkConfig
            let handle = V.resourcesToHandle resources logger
 --           forever (execute handle Vk)
            forever' resources $ mainLoop
                vkConf
                (flip V.resourcesToHandle logger)
                D.log
                V.vkHandlers
                (flip execute Vk)

forever' :: a -> (a -> IO a) -> IO a
forever' res action = do
    res' <- action res
    forever' res' action

mainLoop ::
    d -- config
    -> (a -> b) -- resources to handlers
    -> (b -> L.Handle IO) -- handlers to logger
    -> (L.Handle IO -> d -> a -> [C.Handler IO a]) -- error handlers
    -> (b -> IO ()) -- handlers to execute-action
    -> a -- resources
    -> IO a
mainLoop conf resourcesToHandles toLogger errorHandlers action resources = do
    let handle = resourcesToHandles resources
        logger = toLogger handle
    (action handle >> return resources)
        `C.catches`
        errorHandlers logger conf resources -- someHandlers

 
runWithConf' :: (BotConfigurable s) => s -> RunOptions -> FilePath -> (EnvironmentCommon -> Conf s -> IO ()) -> IO ()
runWithConf' s opts path todo = do
    let f (E.Handler g) = E.Handler $ \e -> do
            g e
            L.logFatal L.simpleLog 
                "Failed to get required data from configuration files, terminating..."
            Q.exitWith $ Q.ExitFailure 1
    (gen, conf) <- loadConfig s L.simpleLog path `E.catches` map f (configHandlers L.simpleLog)
    L.logDebug L.simpleLog "Successfully got bot configuration."
    when (testConfig opts) $ Q.exitWith (Q.ExitSuccess)
    todo gen conf





configHandlers :: L.Handle IO -> [E.Handler ()]
configHandlers h = 
    let f (E.Handler g) = E.Handler (\e -> g e >>
            L.logFatal h
                "Failed to get required data from configuration files, terminating..."
            >> Q.exitWith (Q.ExitFailure 1))
    in  map f [E.Handler (handleIOError L.simpleLog),
                     E.Handler (handleConfigError L.simpleLog),
                     E.Handler (handleConfig2Error L.simpleLog),
                     E.Handler (handleOthers L.simpleLog)]
 

handleIOError :: L.Handle IO -> E.IOException -> IO ()
handleIOError logger exc
  | E.isDoesNotExistError exc = L.logError logger "File does not exist."
  | E.isPermissionError exc   = L.logError logger "Not enough permissions to open file."
  | E.isAlreadyInUseError exc = L.logError logger "File is already in use."
  | otherwise = L.logError logger "Unknown error occured"

handleConfigError :: L.Handle IO -> CT.ConfigError -> IO ()
handleConfigError logger (CT.ParseError path s) =
    L.logError logger $ "Failed to parse configuration file."

handleConfig2Error :: L.Handle IO -> ConfigException -> IO ()
handleConfig2Error logger RequiredFieldMissing =
    L.logError logger "Failed to get required field value."

handleOthers :: L.Handle IO -> E.SomeException -> IO ()
handleOthers logger exc =
    L.logError logger "Unknown error occured."


