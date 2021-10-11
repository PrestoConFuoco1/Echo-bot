{-# LANGUAGE
    DeriveGeneric
    #-}
module Types where

import qualified Stuff as S (Timeout)
import Data.IORef (IORef, atomicModifyIORef)
import qualified Data.Text as T (Text)
import GHC.Generics
import qualified GenericPretty as GP
import Data.Aeson.Types (Value)


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

defStateGen = EnvironmentCommon ("Hello! Available commands:\n" <>
                "-- /help - to get help\n" <>
                "-- /set - to change current number of messages repeats")
                "How many times would you like to repeat every reply?" 1 25 
                "/help" "/set"

-------------------------------------------------------------------
data Command = Help | SetRepNum
    deriving (Show, Eq)

data Event h u m b  = ECommand Command (Maybe h) (Maybe u)
                    | EMessage m
                    | ECallback b
                    | EError Value

data CallbQuery u c = CSetRepNum u (Maybe c) Int | CError String

instance Show (Event h u m b) where
    show (ECommand _ _ _) = "Command"
    show (EMessage _ ) = "Message"
    show (ECallback _ ) = "Callback"
    show _ = "error"

-------------------------------------------------------------------

