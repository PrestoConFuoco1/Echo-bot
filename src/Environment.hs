{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Environment
  ( Environment (..),
    EnvMessages (..),
    EnvCommands (..),
    defStateGen,
    getHelpMessage,
    getRepeatQuestion,
    getHelpCommand,
    getSetRepNumCommand,
    getDefaultRepNum,
    getDefaultTimeout,
  )
where

import qualified Data.Text as T (Text)
import GHC.Generics
import qualified GenericPretty as GP
import qualified Stuff as S (Timeout)

data Environment = Environment -- never changes
  { envMessages :: EnvMessages,
    repNum :: Int,
    timeout :: S.Timeout,
    envCommands :: EnvCommands
  }
  deriving stock (Show, Generic)

instance GP.PrettyShow Environment where
  prettyShow =
    GP.genericPrettyShow
      GP.defaultOptionsL
        { GP.consModifier = const "Common settings"
        }

data EnvMessages = EnvMessages
  { helpMsg :: T.Text,
    repQuestion :: T.Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (GP.PrettyShow)

data EnvCommands = EnvCommands
  { helpCommand :: T.Text,
    setRepNumCommand :: T.Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (GP.PrettyShow)

defaultMessages :: EnvMessages
defaultMessages =
  EnvMessages
    { helpMsg =
        "Hello! Available commands:\n\
        \-- /help - to get help\n\
        \-- /set - to change current number of messages repeats",
      repQuestion =
        "How many times would you like to repeat every reply?"
    }

defaultCommands :: EnvCommands
defaultCommands =
  EnvCommands
    { helpCommand = "/help",
      setRepNumCommand = "/set"
    }

defStateGen :: Environment
defStateGen =
  Environment
    { envMessages = defaultMessages,
      repNum = 1,
      timeout = 25,
      envCommands = defaultCommands
    }

getHelpMessage :: Environment -> T.Text
getHelpMessage = helpMsg . envMessages

getRepeatQuestion :: Environment -> T.Text
getRepeatQuestion = repQuestion . envMessages

getHelpCommand :: Environment -> T.Text
getHelpCommand = helpCommand . envCommands

getSetRepNumCommand :: Environment -> T.Text
getSetRepNumCommand = setRepNumCommand . envCommands

getDefaultTimeout :: Environment -> S.Timeout
getDefaultTimeout = timeout

getDefaultRepNum :: Environment -> Int
getDefaultRepNum = repNum
