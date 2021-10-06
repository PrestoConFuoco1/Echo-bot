module App.Logger where

import Prelude hiding (log)
import qualified Data.Text as T (Text, pack, unpack)
import qualified System.IO as S


newtype Handle m = Handle { log :: Priority -> T.Text -> m () }

type LoggerEntry = (Priority, T.Text)

data Priority = Debug
              | Info
              | Warning
              | Error
              | Fatal
            deriving (Eq, Ord, Show)


logDebug, logInfo, logWarning, logError :: Handle m -> T.Text -> m ()
logDebug = (`log` Debug)
logInfo = (`log` Info)
logWarning = (`log` Warning)
logError = (`log` Error)
logFatal = (`log` Fatal)

fileHandleToLogger :: S.Handle -> Handle IO
fileHandleToLogger h = Handle $ fileLogger h

simpleLog :: Handle IO
--simpleLog = Handle $ \p s -> S.hPutStrLn S.stderr $ '[' : show p ++ "]: " ++ T.unpack s
simpleLog = Handle $ fileLogger S.stderr

fileLogger :: S.Handle -> Priority -> T.Text -> IO ()
fileLogger h p s = S.hPutStrLn S.stderr $ '[' : show p ++ "]: " ++ T.unpack s

emptyLogger :: Handle IO
emptyLogger = Handle $ \p s -> return ()
