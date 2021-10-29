{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Environment (EnvironmentCommon(..), EnvMessages(..),
EnvCommands(..), defStateGen, getHelpMessage, getRepeatQuestion,
getHelpCommand, getSetRepNumCommand, getDefaultRepNum, getDefaultTimeout
) where

import qualified Data.Text as T (Text)
import GHC.Generics
import qualified GenericPretty as GP
import qualified Stuff as S (Timeout)

data EnvironmentCommon = EnvironmentCommon -- never changes
  { envMessages :: EnvMessages,
    repNum :: Int,
    timeout :: S.Timeout,
    envCommands :: EnvCommands
  }
  deriving stock (Show, Generic)

instance GP.PrettyShow EnvironmentCommon where
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

defStateGen :: EnvironmentCommon
defStateGen =
  EnvironmentCommon
    { envMessages = defaultMessages,
      repNum = 1,
      timeout = 25,
      envCommands = defaultCommands
    }

getHelpMessage :: EnvironmentCommon -> T.Text
getHelpMessage = helpMsg . envMessages

getRepeatQuestion :: EnvironmentCommon -> T.Text
getRepeatQuestion = repQuestion . envMessages

getHelpCommand :: EnvironmentCommon -> T.Text
getHelpCommand = helpCommand . envCommands

getSetRepNumCommand :: EnvironmentCommon -> T.Text
getSetRepNumCommand = setRepNumCommand . envCommands

getDefaultTimeout :: EnvironmentCommon -> S.Timeout
getDefaultTimeout = timeout

getDefaultRepNum :: EnvironmentCommon -> Int
getDefaultRepNum = repNum


