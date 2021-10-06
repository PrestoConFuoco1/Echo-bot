{-# LANGUAGE OverloadedStrings #-}

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
    simpleHttp
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
import qualified Data.Text.Lazy as TL (Text, pack, unpack, toStrict)
import qualified Data.Text as T (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)

newtype HttpHandle = HttpHandle { sendH :: Bool -> HTTPRequest -> IO (Either String BSL.ByteString) }

simpleHttp = HttpHandle sendRequest

data HTTPMethod = GET | POST deriving (Show, Eq)


handleHTTPError :: HttpException -> IO (Either String a)
handleHTTPError (HttpExceptionRequest _ content) = return . Left . show $ content
handleHTTPError (InvalidUrlException _ _) = return . Left $ "invalid URL"

handleOthers :: SomeException -> IO (Either String a)
handleOthers e = return . Left $ "unknown error occured"

------------------------------------------------------------------

data ParVal = PIntg Integer
            | PFloat Double
--            | PStr String
            | PVal Ae.Value
            | PText TL.Text
        deriving (Show, Eq)

instance Ae.ToJSON ParVal where
    toJSON (PIntg n) = Ae.toJSON n
--    Ae.toJSON (PStr s)  = Ae.toJSON s
    toJSON (PVal v)  = v
    toJSON (PFloat x) = Ae.toJSON x
    toJSON (PText x) = Ae.toJSON x

parValToString :: ParVal -> String
parValToString (PIntg n) = show n
--parValToString (PStr s)  = s
parValToString (PVal v) = TL.unpack . decodeUtf8 . Ae.encode $ v
parValToString (PFloat x) = show x
parValToString (PText x) = TL.unpack x

type ParamsUnit = (TL.Text, Maybe ParVal)
type ParamsList = [ParamsUnit]

data HTTPRequest = Req {
    mthd :: HTTPMethod,
    reqUrl :: TL.Text,
    pars :: ParamsList } deriving (Show, Eq)



sendRequest :: Bool -> HTTPRequest -> IO (Either String BSL.ByteString)
sendRequest takesJSON (Req method url params) =
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
    in  case parsedReq of
            Nothing -> return . Left $ "Couldn't parse the HTTP request" ++ req
            Just parsedReq ->
                (fmap (Right . getResponseBody) . httpLBS) parsedReq
                    `catches` [Handler handleHTTPError, Handler handleOthers]

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

