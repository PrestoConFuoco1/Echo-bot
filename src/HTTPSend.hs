module HTTPSend
(   sendRequest
) where
 
import HTTPTypes
import qualified App.Logger as L
import Control.Exception (Handler (..), catches)
import qualified Data.Aeson as Ae
  (
    Value,
    object,
    (.=)
  )
import qualified Data.ByteString.Lazy.Char8 as BSL
  ( ByteString,
  )
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T (unpack)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Simple
  ( HttpException (..),
    Request,
    getResponseBody,
    httpLBS,
    parseRequest,
    setRequestBodyJSON,
  )



handleHTTPError :: HttpException -> IO (Either String a)
handleHTTPError (HttpExceptionRequest _ content) =
  pure . Left . show $ content
handleHTTPError (InvalidUrlException _ _) =
  pure $ Left "invalid URL"


sendRequest ::
  L.LoggerHandler IO ->
  Bool ->
  HTTPRequest ->
  IO (Either String BSL.ByteString)
sendRequest h takesJSON r = do
  let (req, parsedReq) = buildRequest takesJSON r
  L.logDebug h (showTReq r)
  case parsedReq of
    Nothing ->
      pure . Left $
        "Couldn't parse the HTTP request" ++ req
    Just parsedReq' ->
      (fmap (Right . getResponseBody) . httpLBS)
        parsedReq'
        `catches` [Handler handleHTTPError]

buildRequest :: Bool -> HTTPRequest -> (String, Maybe Request)
buildRequest takesJSON (Req method url params) =
  case method of
    GET -> (reqWithoutJSON, parsedReqWithoutJSON)
    POST ->
      if takesJSON
        then (reqJSON, parsedReqJSON)
        else
          ( reqWithoutJSON,
            parsedReqWithoutJSON
          )
  where
    reqWithoutJSON =
      show method
        ++ ' ' :
      T.unpack url ++ makeParamsString params
    parsedReqWithoutJSON = parseRequest reqWithoutJSON
    reqJSON = show method ++ ' ' : T.unpack url
    parsedReqJSON = f <$> parseRequest reqJSON
    f = setRequestBodyJSON (makeParamsValue params)

makeParamsString :: ParamsList -> String
makeParamsString lst =
  let paramsList = (intercalate "&" . mapMaybe f) lst
      f (s, x) =
        (\q -> T.unpack s ++ "=" ++ q)
          . urlEncode
          . parValToString
          <$> x
   in if null paramsList
        then ""
        else '?' : paramsList

makeParamsValue :: ParamsList -> Ae.Value
makeParamsValue lst =
  let lst' = mapMaybe f lst
      f (s, x) = fmap (s Ae..=) x
   in Ae.object lst'


