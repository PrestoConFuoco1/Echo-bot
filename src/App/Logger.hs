module App.Logger where

import Prelude hiding (log)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.IO as T (hPutStrLn)
import qualified System.IO as S
import Text.Read
import Control.Monad (when)
import qualified Stuff as S (showT)
import Data.IORef
import qualified Control.Monad.Catch as C
import qualified System.Exit as Q (ExitCode (..), exitWith)
import qualified System.IO.Error as IOE

newtype Handle m = Handle { log :: Priority -> T.Text -> m () }

type LoggerEntry = (Priority, T.Text)

data Priority = Debug
              | Info
              | Warning
              | Error
              | Fatal
            deriving (Eq, Ord, Show, Read)


logDebug, logInfo, logWarning, logError :: Handle m -> T.Text -> m ()
logDebug = (`log` Debug)
logInfo = (`log` Info)
logWarning = (`log` Warning)
logError = (`log` Error)
logFatal = (`log` Fatal)

logString :: Priority -> T.Text -> T.Text
logString pri s = "[" <> S.showT pri <> "]: " <> s

simpleHandle = simpleCondHandle (const True)

simpleCondHandle :: (Priority -> Bool) -> Handle IO
--simpleLog = Handle $ \p s -> S.hPutStrLn S.stderr $ '[' : show p ++ "]: " ++ T.unpack s
simpleCondHandle pred = Handle $ \p s -> when (pred p) $ simpleLogger p s

simpleLogger = fileLogger

fileLogger :: Priority -> T.Text -> IO ()
fileLogger p s = --T.hPutStrLn S.stderr $ '[' : show p ++ "]: " ++ T.unpack s
    T.hPutStrLn S.stderr $ logString p s

emptyLogger :: (Monad m) => Handle m
emptyLogger = Handle $ \p s -> return ()


data LoggerConfig = LoggerConfig {
    lcFilter :: Priority -> Bool
    , lcPath :: FilePath
    }

data LoggerResources = LoggerResources {
    flHandle :: S.Handle
    }

pathToHandle :: FilePath -> IO S.Handle
pathToHandle path = do
    h <- S.openFile path S.AppendMode
    return h

initializeErrorHandler :: IOE.IOError -> IO a
initializeErrorHandler e = func e >> Q.exitWith (Q.ExitFailure 1)
  where
   func e
    | IOE.isAlreadyInUseError e = logError simpleHandle "target log file is locked"
    | IOE.isPermissionError e   = logError simpleHandle "not enough permissions"
    | otherwise = logError simpleHandle $ "unexpected IO error: " <> T.pack (C.displayException e)


initializeDefaultHandler :: C.SomeException -> IO a
initializeDefaultHandler e = do
    logFatal simpleHandle $ "failed to initialize logger"
    logFatal simpleHandle $ T.pack $ C.displayException e
    Q.exitWith (Q.ExitFailure 1)

initializeSelfSufficientLogger :: LoggerConfig -> IO (Handle IO)
initializeSelfSufficientLogger conf = do
    h <- pathToHandle (lcPath conf) `C.catches`
        [C.Handler initializeErrorHandler,
         C.Handler initializeDefaultHandler]
    let resources = LoggerResources {
            flHandle = h
            }
    resourcesRef <- newIORef resources
    return $ Handle $ selfSufficientLogger resourcesRef (lcFilter conf)

selfSufficientLogger :: IORef LoggerResources -> (Priority -> Bool) -> Priority -> T.Text -> IO ()
selfSufficientLogger resourcesRef pred pri s = do
    resources <- readIORef resourcesRef
    let action = T.hPutStrLn (flHandle resources) (logString pri s)
        errHandler = \e -> loggerHandler resources e >>= writeIORef resourcesRef
    action `C.catch` errHandler

loggerHandler :: LoggerResources -> C.SomeException -> IO LoggerResources
loggerHandler resources e = do
    logError simpleHandle "failed to use log file, error is:"
    logError simpleHandle $ T.pack $ C.displayException e
    logError simpleHandle "using standard error handle"
    return $ resources { flHandle = S.stderr }




