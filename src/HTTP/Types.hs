module HTTP.Types
    ( ParamsList
    , ParamsUnit
    , HTTPMethod(..)
    , HTTPRequest(..)
    , ParVal(..)
    , ToParVal(..)
    , unit
    , mUnit
    , showTReq
    , parValToString
    , addParams
    , addParamsUnit
    , buildHTTP
    ) where

import qualified Data.Aeson as Ae (ToJSON(..), Value, encode)
import qualified Data.ByteString.Lazy as BS (toStrict)
import qualified Data.Text as T (Text, concat, unpack)
import Data.Text.Encoding as E (decodeUtf8)
import qualified Stuff as S

data HTTPMethod
    = GET
    | POST
  deriving (Show, Eq)

------------------------------------------------------------------
data ParVal
    = PIntg Integer
    | PFloat Double
    | PVal Ae.Value
    | PLText T.Text
    | PText T.Text
  deriving (Eq)

instance Ae.ToJSON ParVal where
    toJSON (PIntg n) = Ae.toJSON $ S.showT n
    toJSON (PVal v) = v
    toJSON (PFloat x) = Ae.toJSON $ S.showT x
    toJSON (PLText x) = Ae.toJSON x
    toJSON (PText x) = Ae.toJSON x

parValToString :: ParVal -> String
parValToString (PIntg n) = show n
parValToString (PVal v) =
    T.unpack . E.decodeUtf8 . BS.toStrict . Ae.encode $ v
parValToString (PFloat x) = show x
parValToString (PLText x) = T.unpack x
parValToString (PText x) = T.unpack x

instance Show ParVal where
    show = parValToString

type ParamsUnit = (T.Text, Maybe ParVal)

type ParamsList = [ParamsUnit]

data HTTPRequest =
    Req
        { mthd :: HTTPMethod
        , reqUrl :: T.Text
        , pars :: ParamsList
        }
  deriving (Show, Eq)

showTReq :: HTTPRequest -> T.Text
showTReq (Req method url params) =
    T.concat
        [ "\n    method: "
        , S.showT method
        , "\n    URL: "
        , S.showT url
        , "\n"
        , showTParams params
        ]

showTParams :: ParamsList -> T.Text
showTParams = T.concat . map f
  where
    f (_, Nothing) = ""
    f (field, Just value) =
        T.concat ["    ", field, ": ", S.showT value, "\n"]

-------------------------------------------------------------------
class ToParVal a where
    toParVal :: a -> ParVal

instance ToParVal Integer where
    toParVal = PIntg

instance ToParVal Double where
    toParVal = PFloat

instance ToParVal Ae.Value where
    toParVal = PVal

instance ToParVal T.Text where
    toParVal = PLText

instance (Ae.ToJSON a) => ToParVal [a] where
    toParVal = PVal . Ae.toJSON

unit :: (ToParVal a) => T.Text -> a -> ParamsUnit
unit field value = (field, Just $ toParVal value)

mUnit :: (ToParVal a) => T.Text -> Maybe a -> ParamsUnit
mUnit field mValue = (field, fmap toParVal mValue)

------------------------------------------------------
addParams :: ParamsList -> HTTPRequest -> HTTPRequest
addParams params req = req {pars = params ++ pars req}

addParamsUnit :: ParamsUnit -> HTTPRequest -> HTTPRequest
addParamsUnit paramsUnit req = req {pars = paramsUnit : pars req}


buildHTTP :: T.Text -> (T.Text, ParamsList) -> HTTPRequest
buildHTTP url (method, params) = Req POST (url <> method) params


