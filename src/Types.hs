{-# LANGUAGE
    DeriveGeneric
    , DeriveAnyClass
    #-}
module Types where

import qualified Stuff as S (Timeout)
import qualified Data.Text as T (Text)
import GHC.Generics
import qualified GenericPretty as GP
import Data.Aeson.Types (Value)
import GenericPretty

data EnvironmentCommon = EnvironmentCommon { -- never changes
    helpMsg :: T.Text,
    repQuestion :: T.Text,
    repNum :: Int,
    timeout :: S.Timeout,

    helpCommand :: T.Text,
    setRepNumCommand :: T.Text
    } deriving (Show, Generic)

instance GP.PrettyShow EnvironmentCommon where
    prettyShow = GP.genericPrettyShow GP.defaultOptionsL {
        GP.consModifier = const "Common settings"
        }

defStateGen :: EnvironmentCommon
defStateGen = EnvironmentCommon {
    helpMsg = "Hello! Available commands:\n\
                \-- /help - to get help\n\
                \-- /set - to change current number of messages repeats"
    , repQuestion = "How many times would you like to repeat every reply?"
    , repNum = 1
    , timeout = 25
    , helpCommand = "/help"
    , setRepNumCommand = "/set"
    }
-------------------------------------------------------------------
data Command = Help | SetRepNum
    deriving (Show, Eq)

data Event h u m b  = ECommand Command (Maybe h) (Maybe u)
                    | EMessage m
                    | ECallback b
                    | EError Value

data CallbQuery u c = CSetRepNum u (Maybe c) Int | CError String

data UpdateResponse r e =
    UpdateResponse r
    | UpdateError e
    deriving (Show, Generic, PrettyShow)

instance Show (Event h u m b) where
    show (ECommand _ _ _) = "Command"
    show (EMessage _ ) = "Message"
    show (ECallback _ ) = "Callback"
    show _ = "Unexpected event"

