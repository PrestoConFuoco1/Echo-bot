{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module RunOptions
    ( RunOptions(..)
    , getOptsIO
    , ghciRunOpts
    , toLoggerFilter
    ) where

import qualified App.Logger as L
import qualified Data.Text as T
import GHC.Generics
import qualified GenericPretty as P
import qualified Messenger as M
import Options.Applicative

data LoggerSettings
    = LogAll
    | LogGreaterThan L.Priority
  deriving (Show, Eq)
  deriving P.PrettyShow via P.Showable LoggerSettings

toLoggerFilter :: LoggerSettings -> (L.Priority -> Bool)
toLoggerFilter LogAll = const True
toLoggerFilter (LogGreaterThan pri) = (>= pri)

data RunOptions =
    RunOptions
        { confPath :: T.Text
        , messenger :: M.Messenger
        , loggerSettings :: LoggerSettings
        , logPath :: T.Text
        , testConfig :: Bool
        }
  deriving (Show, Eq, Generic)

instance P.PrettyShow RunOptions

--for ghci
ghciRunOpts :: RunOptions
ghciRunOpts =
    RunOptions
        { confPath = "src/bot.conf"
        , messenger = M.Telegram
        , loggerSettings = LogAll
        , logPath = "./log"
        , testConfig = False
        }

getOpts :: Parser RunOptions
getOpts =
    RunOptions <$> argument str (metavar "CONFPATH") <*>
    (vkMessenger <|> telegramMessenger) <*>
    ((getLoggerSettings <$>
      option
          auto
          (short 'l' <> metavar "LOGLEVEL" <> help "Log level")) <|>
     pure LogAll) <*>
    (strOption
         (long "logpath" <> metavar "LOGFILE" <> help "Log path") <|>
     pure "./log") <*>
    switch (long "test-config" <> help "Test configuration")

botHeader :: String
botHeader = "Echo-bot for Telegram and Vk"

--botDescription = ""
getOptsIO :: IO RunOptions
getOptsIO =
    execParser $
    info
        (getOpts <**> helper)
        (fullDesc
          --    <> progDesc botDescription
          <>
         header botHeader)

vkMessenger :: Parser M.Messenger
vkMessenger = flag' M.Vkontakte (long "vk" <> help "Vk bot")

telegramMessenger :: Parser M.Messenger
telegramMessenger =
    flag'
        M.Telegram
        (long "tl" <> long "telegram" <> help "Telegram bot")

getLoggerSettings :: L.Priority -> LoggerSettings
getLoggerSettings = LogGreaterThan
