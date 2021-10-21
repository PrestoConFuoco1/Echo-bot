module App.Logger where

import Prelude hiding (log)
import qualified Data.Text as T (Text, pack)
import qualified Data.Text.IO as T (hPutStrLn)
import qualified System.IO as S
import Control.Monad (when)
import qualified Stuff as S (showT)
import Data.IORef
import qualified Control.Monad.Catch as C
import qualified System.Exit as Q (ExitCode (..), exitWith)
import qualified System.IO.Error as IOE
import qualified GHC.IO.Handle.Lock as Lk

newtype Handle m = Handle { log :: Priority -> T.Text -> m () }

type LoggerEntry = (Priority, T.Text)

data Priority = Debug
              | Info
              | Warning
              | Error
              | Fatal
            deriving (Eq, Ord, Show, Read)


logDebug, logInfo, logWarning, logError, logFatal :: Handle m -> T.Text -> m ()
logDebug = (`log` Debug)
logInfo = (`log` Info)
logWarning = (`log` Warning)
logError = (`log` Error)
logFatal = (`log` Fatal)

logString :: Priority -> T.Text -> T.Text
logString pri s = "[" <> S.showT pri <> "]: " <> s

simpleHandle :: Handle IO
simpleHandle = simpleCondHandle (const True)

simpleCondHandle :: (Priority -> Bool) -> Handle IO
simpleCondHandle predicate = Handle $ \p s -> when (predicate p) $ stdLogger p s


stdLogger :: Priority -> T.Text -> IO ()
stdLogger p s = T.hPutStrLn S.stderr $ logString p s

emptyLogger :: Handle IO
emptyLogger = Handle $ \_ _ -> return ()




data LoggerConfig = LoggerConfig {
    lcFilter :: Priority -> Bool
    , lcPath :: FilePath
    }

data LoggerResources = LoggerResources {
    flHandle :: S.Handle
    }

pathToHandle :: FilePath -> IO S.Handle
pathToHandle path = S.openFile path S.AppendMode

initializeErrorHandler :: IOE.IOError -> IO a
initializeErrorHandler e = do
    logFatal simpleHandle "failed to initialize logger"
    func e
    Q.exitWith (Q.ExitFailure 1)
  where
   func err
    | IOE.isAlreadyInUseError err = logError simpleHandle lockedmsg
    | IOE.isPermissionError err   = logError simpleHandle "not enough permissions"
    | otherwise = logError simpleHandle $ "unexpected IO error: " <> T.pack (C.displayException err)

lockedmsg :: T.Text
lockedmsg = "target log file is locked"

initializeDefaultHandler :: C.SomeException -> IO a
initializeDefaultHandler e = do
    logFatal simpleHandle "failed to initialize logger"
    logFatal simpleHandle $ T.pack $ C.displayException e
    Q.exitWith (Q.ExitFailure 1)


withSelfSufficientLogger :: LoggerConfig -> (Handle IO -> IO a) -> IO a
withSelfSufficientLogger conf action = do
    C.bracket
        (initializeSelfSufficientLoggerResources conf)
        closeSelfSufficientLogger
        (\resourcesRef -> action $
            Handle $ selfSufficientLogger resourcesRef $
                lcFilter conf)



initializeSelfSufficientLoggerResources :: LoggerConfig -> IO (IORef LoggerResources)
initializeSelfSufficientLoggerResources conf = do
    h <- pathToHandle (lcPath conf) `C.catches`
        [C.Handler initializeErrorHandler,
         C.Handler initializeDefaultHandler]
    lockAcquired <- Lk.hTryLock h Lk.ExclusiveLock
    when (not lockAcquired) $ do
        logFatal simpleHandle "failed to initialize logger"
        logFatal simpleHandle lockedmsg
        Q.exitWith (Q.ExitFailure 1)
        
    let resources = LoggerResources {
            flHandle = h
            }
    resourcesRef <- newIORef resources
    return resourcesRef

closeSelfSufficientLogger :: IORef LoggerResources -> IO ()
closeSelfSufficientLogger resourcesRef = do
    resources <- readIORef resourcesRef
    let h = flHandle resources
    Lk.hUnlock h
    S.hClose h


selfSufficientLogger :: IORef LoggerResources -> (Priority -> Bool) -> Priority -> T.Text -> IO ()
selfSufficientLogger resourcesRef predicate pri s = do
    resources <- readIORef resourcesRef
    let action = T.hPutStrLn (flHandle resources) (logString pri s)
                 >> T.hPutStrLn S.stderr (logString pri s)
        errHandler e = loggerHandler resources e >>= writeIORef resourcesRef
    when (predicate pri) action `C.catch` errHandler

loggerHandler :: LoggerResources -> C.SomeException -> IO LoggerResources
loggerHandler resources e = do
    logError simpleHandle "failed to use log file, error is:"
    logError simpleHandle $ T.pack $ C.displayException e
    logError simpleHandle "using standard error handle"
    return $ resources { flHandle = S.stderr }




