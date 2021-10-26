module HTTPRequests
   ( sendRequest
   , ParamsList
   , ParamsUnit
   , HTTPMethod(..)
   , HTTPRequest(..)
   , addParams
   , addParamsUnit
   , ParVal(..)
   , ToParVal(..)
   , unit
   , mUnit
   ) where

import qualified App.Logger as L
import qualified Data.ByteString.Lazy as BS (toStrict)
import Control.Exception (Handler(..), catches)
import qualified Data.Aeson as Ae
   ( ToJSON(..)
   , Value
   , (.=)
   , encode
   , object
   )
import qualified Data.ByteString.Lazy.Char8 as BSL
   ( ByteString)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T (Text, concat, unpack)
import Data.Text.Encoding as E (decodeUtf8)

import Network.HTTP.Base (urlEncode)
import Network.HTTP.Simple
   ( HttpException(..)
   , Request
   , getResponseBody
   , httpLBS
   , parseRequest
   , setRequestBodyJSON
   )
import qualified Stuff as S

data HTTPMethod
   = GET
   | POST
   deriving (Show, Eq)

handleHTTPError :: HttpException -> IO (Either String a)
handleHTTPError (HttpExceptionRequest _ content) =
   pure . Left . show $ content
handleHTTPError (InvalidUrlException _ _) =
   pure $ Left "invalid URL"

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

sendRequest ::
      L.LoggerHandler IO
   -> Bool
   -> HTTPRequest
   -> IO (Either String BSL.ByteString)
sendRequest h takesJSON r = do
   let (req, parsedReq) = buildRequest takesJSON r
   L.logDebug h (showTReq r)
   case parsedReq of
          Nothing ->
             pure . Left $
             "Couldn't parse the HTTP request" ++ req
          Just parsedReq' ->
             (fmap (Right . getResponseBody) . httpLBS)
                parsedReq' `catches`
             [Handler handleHTTPError]

buildRequest :: Bool -> HTTPRequest -> (String, Maybe Request)
buildRequest takesJSON (Req method url params) =
    case method of
             GET -> (reqWithoutJSON, parsedReqWithoutJSON)
             POST ->
                if takesJSON
                   then (reqJSON, parsedReqJSON)
                   else ( reqWithoutJSON
                        , parsedReqWithoutJSON)
  where
       reqWithoutJSON =
          show method ++
          ' ' : T.unpack url ++ makeParamsString params
       parsedReqWithoutJSON = parseRequest reqWithoutJSON
       reqJSON = show method ++ ' ' : T.unpack url
       parsedReqJSON = f <$> parseRequest reqJSON
       f = setRequestBodyJSON (makeParamsValue params)


makeParamsString :: ParamsList -> String
makeParamsString lst =
   let paramsList = (intercalate "&" . mapMaybe f) lst
       f (s, x) =
          (\q -> T.unpack s ++ "=" ++ q) .
           urlEncode . parValToString <$>
          x
    in if null paramsList
          then ""
          else '?' : paramsList

makeParamsValue :: ParamsList -> Ae.Value
makeParamsValue lst =
   let lst' = mapMaybe f lst
       f (s, x) = fmap (s Ae..= ) x
    in Ae.object lst'

addParams :: ParamsList -> HTTPRequest -> HTTPRequest
addParams params req = req {pars = params ++ pars req}

addParamsUnit :: ParamsUnit -> HTTPRequest -> HTTPRequest
addParamsUnit paramsUnit req =
   req {pars = paramsUnit : pars req}

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
