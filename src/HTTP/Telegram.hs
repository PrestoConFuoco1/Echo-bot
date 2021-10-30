module HTTP.Telegram
    ( getUpdates
    , sendThis
    ) where

import qualified App.Logger as L
import qualified Data.Aeson as Ae (decode)
import Data.Aeson.Types as AeT
import qualified Data.ByteString.Lazy.Char8 as BSL (ByteString)
import qualified HTTP.Send as H
import qualified HTTP.Types as H
import Telegram (tlTakesJSON)
import Telegram.Update
    ( TlReply
    , TlUpdateReplyError
    , TlUpdateReplySuccess
    )
import qualified Telegram.Update as U (parseUpdatesResponse)

parseUpdatesResponse ::
       BSL.ByteString
    -> Either String (Either TlUpdateReplyError TlUpdateReplySuccess)
parseUpdatesResponse resp -- Either
 = do
    val <-
        maybe
            (Left "Couldn't parse updates response into aeson Value")
            Right $
        Ae.decode resp
    U.parseUpdatesResponse val

getUpdates ::
       L.LoggerHandler IO
    -> H.HTTPRequest
    -> IO (Either String (Either TlUpdateReplyError TlUpdateReplySuccess))
getUpdates logger request = do
    let takesJSON = True
    eithRespStr <- H.sendRequest logger takesJSON request -- Either String a
    pure $ eithRespStr >>= parseUpdatesResponse

parseHTTPResponse :: BSL.ByteString -> Either String TlReply
parseHTTPResponse resp -- Either
 = do
    val <-
        maybe (Left "Couldn't parse HTTP response") Right $
        Ae.decode resp
    parseEither parseJSON val

sendThis ::
       L.LoggerHandler IO
    -> H.HTTPRequest
    -> IO (Either String TlReply)
sendThis logger request = do
    let takesJSON = tlTakesJSON
    eithRespStr <- H.sendRequest logger takesJSON request
    pure $ eithRespStr >>= parseHTTPResponse
