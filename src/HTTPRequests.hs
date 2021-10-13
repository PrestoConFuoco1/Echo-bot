{-# LANGUAGE
    OverloadedStrings
    , DeriveGeneric
    , DeriveAnyClass
    #-}

module HTTPRequests (
    sendRequest,
    ParamsList,
    ParamsUnit,
    HTTPMethod (..),
    HTTPRequest (..),
    addParams,
    addParamsUnit,
    ParVal (..),
    HttpHandle (..),
    --simpleHttp
    ToParVal (..),
    unit,
    mUnit
) where

import Prelude hiding (log)
import qualified Data.ByteString.Lazy.Char8 as BSL (ByteString) --, toStrict)
import Network.HTTP.Simple (HttpException(..), parseRequest, setRequestBodyJSON, getResponseBody, httpLBS)
import Network.HTTP.Base (urlEncode)
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import Control.Exception (catches, SomeException, Handler(..))
import Data.Bifunctor (bimap)
import qualified Data.Aeson as Ae (ToJSON (..), Value, encode, object, (.=))
import qualified Data.Text.Lazy as TL (Text, pack, unpack, toStrict, concat, toStrict)
import qualified Data.Text as T (Text, unpack, concat)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified GenericPretty as GP
import GHC.Generics
import qualified Stuff as S
import qualified App.Logger as L

newtype HttpHandle = HttpHandle { sendH :: Bool -> HTTPRequest -> IO (Either String BSL.ByteString) }

--simpleHttp = HttpHandle sendRequest

data HTTPMethod = GET | POST deriving (Show, Eq)



handleHTTPError :: HttpException -> IO (Either String a)
handleHTTPError (HttpExceptionRequest _ content) = return . Left . show $ content
handleHTTPError (InvalidUrlException _ _) = return . Left $ "invalid URL"

handleOthers :: SomeException -> IO (Either String a)
handleOthers e = return . Left $ "unknown error occured"

------------------------------------------------------------------

data ParVal = PIntg Integer
            | PFloat Double
            | PVal Ae.Value
            | PLText TL.Text
            | PText T.Text
        deriving (Eq)

instance Ae.ToJSON ParVal where
    toJSON (PIntg n) = Ae.toJSON $ S.showT n
    toJSON (PVal v)  = v
    toJSON (PFloat x) = Ae.toJSON $ S.showT x
    toJSON (PLText x) = Ae.toJSON x
    toJSON (PText x) = Ae.toJSON x

parValToString :: ParVal -> String
parValToString (PIntg n) = show n
--parValToString (PStr s)  = s
parValToString (PVal v) = TL.unpack . decodeUtf8 . Ae.encode $ v
parValToString (PFloat x) = show x
parValToString (PLText x) = TL.unpack x
parValToString (PText x) = T.unpack x

instance Show ParVal where
    show = parValToString

type ParamsUnit = (TL.Text, Maybe ParVal)
type ParamsList = [ParamsUnit]

data HTTPRequest = Req {
    mthd :: HTTPMethod,
    reqUrl :: TL.Text,
    pars :: ParamsList
    } deriving (Show, Eq)

showTReq :: HTTPRequest -> T.Text
showTReq (Req method url params) =
    T.concat ["\n    method: ", S.showT method,
    "\n    URL: ", S.showT url, "\n", TL.toStrict $ showTParams params]
showTParams :: ParamsList -> TL.Text
showTParams = TL.concat . map f
  where f (field, Nothing) = ""
        f (field, Just value) = TL.concat ["    ", field, ": ", S.showTL value, "\n"]



sendRequest :: L.Handle IO -> Bool -> HTTPRequest -> IO (Either String BSL.ByteString)
sendRequest h takesJSON r@(Req method url params) =
    let (req, parsedReq) = case method of
            GET -> (reqWithoutJSON, parsedReqWithoutJSON)
            POST -> if takesJSON
                    then (reqJSON, parsedReqJSON)
                    else (reqWithoutJSON, parsedReqWithoutJSON)
        reqWithoutJSON = show method ++ ' ' : TL.unpack url ++ makeParamsString params
        parsedReqWithoutJSON = parseRequest reqWithoutJSON
        reqJSON = show method ++ ' ' : TL.unpack url
        parsedReqJSON = fmap f $ parseRequest $ reqJSON

        f r = setRequestBodyJSON (makeParamsValue params) r
    in  L.logDebug h (showTReq r) >> case parsedReq of
            Nothing -> return . Left $ "Couldn't parse the HTTP request" ++ req
            Just parsedReq ->
                (fmap (Right . getResponseBody) . httpLBS) parsedReq
                    `catches`
                    [
                    Handler handleHTTPError
                    -- , Handler handleOthers
                    ]

makeParamsString :: ParamsList -> String
makeParamsString lst =
    let paramsList = (intercalate "&" . catMaybes . map f) lst
        f (s, x) = fmap (\q -> TL.unpack s ++ "=" ++ q) $ fmap (urlEncode . parValToString) x
    in  if null paramsList then "" else '?':paramsList

makeParamsValue :: ParamsList -> Ae.Value
makeParamsValue lst =
    let lst' = catMaybes $ map f lst
        f (s, x) = fmap (\q -> TL.toStrict s Ae..= q) x
    in  Ae.object lst'


addParams :: ParamsList -> HTTPRequest -> HTTPRequest
addParams params req = req { pars = params ++ pars req}

addParamsUnit :: ParamsUnit -> HTTPRequest -> HTTPRequest
addParamsUnit paramsUnit req = req { pars = paramsUnit : pars req }

{-
-}
-------------------------------------------------------------------

class ToParVal a where
    toParVal :: a -> ParVal

instance ToParVal Integer where
    toParVal = PIntg
instance ToParVal Double where
    toParVal = PFloat
instance ToParVal Ae.Value where
    toParVal = PVal
instance ToParVal TL.Text where
    toParVal = PLText
instance ToParVal T.Text where
    toParVal = PText

unit :: (ToParVal a) => TL.Text -> a -> ParamsUnit
unit field value = (field, Just $ toParVal value)

mUnit :: (ToParVal a) => TL.Text -> (Maybe a) -> ParamsUnit
mUnit field mValue = (field, fmap toParVal mValue)


