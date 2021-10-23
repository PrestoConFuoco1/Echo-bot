{-# LANGUAGE DeriveGeneric #-}
module RunOptions where

import GHC.Generics
import qualified App.Logger as L
import qualified Stuff as S (withMaybe)
import Text.Read (readMaybe)
import Data.List (isPrefixOf)
import Options.Applicative
import Data.Maybe (fromMaybe)
import qualified GenericPretty as P
import qualified Data.Text as T
import Types

data LoggerSettings =
    LogAll
    | LogGreaterThan L.Priority
    deriving (Show, Eq)
instance P.PrettyShow LoggerSettings where
    prettyShow = P.LStr . show

toLoggerFilter :: LoggerSettings -> (L.Priority -> Bool)
toLoggerFilter LogAll = const True
toLoggerFilter (LogGreaterThan pri) = (>= pri)

data RunOptions =
   RunOptions
      { confPath :: T.Text
      , messenger :: Messenger
      , loggerSettings :: LoggerSettings
      , logPath :: T.Text
      , testConfig :: Bool
      } deriving (Show, Eq, Generic)
instance P.PrettyShow RunOptions

--for ghci
ghciRunOpts :: RunOptions
ghciRunOpts =
   RunOptions
      { confPath = "./bot.conf"
      , messenger = Telegram
      , loggerSettings = LogAll
      , logPath = "./log"
      , testConfig = False
      }

getOpts :: Parser RunOptions
getOpts = RunOptions
    <$> argument str (metavar "CONFPATH")
    <*> (vkMessenger <|> telegramMessenger)
    <*> (
        (getLoggerSettings <$> option auto ( short 'l' <> metavar "LOGLEVEL" <> help "Log level" ))
        <|> pure LogAll)
    <*> ( 
        strOption ( long "logpath" <> metavar "LOGFILE" <> help "Log path")
        <|> pure "./log")
    <*> switch (long "test-config" <> help "Test configuration")

botHeader = "Echo-bot for Telegram and Vk"
--botDescription = ""

getOptsIO :: IO RunOptions
getOptsIO = execParser $ info (getOpts <**> helper)
    ( fullDesc
--    <> progDesc botDescription
    <> header   botHeader )

vkMessenger :: Parser Messenger
vkMessenger = flag' Vkontakte (long "vk" <> help "Vk bot")

telegramMessenger :: Parser Messenger
telegramMessenger = flag' Telegram (long "tl" <> long "telegram" <> help "Telegram bot")

getLoggerSettings :: L.Priority -> LoggerSettings
getLoggerSettings = LogGreaterThan
