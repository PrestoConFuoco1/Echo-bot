module Lib
    ( someFunc
    ) where


--import qualified Stuff as S (echo, findWithDefault, safeHead, showT)
--import qualified HTTPRequests as H
import Config

import Data.IORef (newIORef)
import qualified Data.Map as M (Map, empty, insert, findWithDefault)
import Data.Foldable (asum)
import qualified Control.Exception as E
    (catches, Handler (..), SomeException, IOException)
import qualified System.IO.Error as E
    (isDoesNotExistError, isPermissionError, isAlreadyInUseError)

import qualified Data.Configurator.Types as CT (ConfigError (..))

import qualified System.Exit as Q (ExitCode (..), exitWith)

import qualified Data.Text.Lazy as T (Text, pack, words, unpack)
--import Text.Read (readMaybe)

import qualified GenericPretty as GP
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import qualified App.Logger as L
import Control.Monad (when, forever)
import Execute

import qualified App.Handle.Telegram as T

data RunOptions = RunOptions {
    testConfig :: Bool
    }
defaultRunOpts = RunOptions { testConfig = False }


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
        f _ acc = acc

runWithConf :: RunOptions -> FilePath -> IO ()
runWithConf opts path = do
    let f (E.Handler g) = E.Handler (\e -> g e >>
            L.logFatal L.simpleLog 
                "Failed to get required data from configuration files, terminating..."
            >> Q.exitWith (Q.ExitFailure 1))
    (gen, eithConf) <- loadConfig L.simpleLog path `E.catches` map f (configHandlers L.simpleLog)
    L.logDebug L.simpleLog "Successfully got bot configuration."
    when (testConfig opts) $ Q.exitWith (Q.ExitSuccess)

    case eithConf of
        TlC tlConf -> do
            let tlConfig = T.Config gen tlConf
            resources <- T.initResources tlConfig
            let handle = T.resourcesToHandle resources L.simpleLog
            forever (mainLoop handle undefined)
{-
    case eithConf of
        TlC tlConf -> runBot dummyTl L.simpleLog H.simpleHttp gen tlConf
        VkC vkConf -> runBot dummyVk L.simpleLog H.simpleHttp gen vkConf
-}








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


