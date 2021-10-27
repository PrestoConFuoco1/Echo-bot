module Telegram.Send
  ( getUpdates,
    sendThis,
  )
where

import qualified App.Logger as L
import qualified Data.Aeson as Ae (decode)
import Data.Aeson.Types as AeT
import qualified Data.ByteString.Lazy.Char8 as BSL
  ( ByteString,
  )
import qualified HTTPTypes as H
import qualified HTTPSend as H
import Telegram.General (tlTakesJSON)
import Telegram.Update (TlReply, TlUpdateReplyError, TlUpdateReplySuccess, parseUpdatesResponse1)
import qualified Types as Y

parseUpdatesResponse ::
  BSL.ByteString ->
  Either String (Y.UpdateResponse TlUpdateReplySuccess TlUpdateReplyError)
parseUpdatesResponse resp -- Either
  =
  do
    val <-
      maybe
        ( Left
            "Couldn't parse updates response into aeson Value"
        )
        Right
        $ Ae.decode resp
    parseUpdatesResponse1 val

getUpdates ::
  L.LoggerHandler IO ->
  H.HTTPRequest ->
  IO (Either String (Y.UpdateResponse TlUpdateReplySuccess TlUpdateReplyError))
getUpdates logger request = do
  let takesJSON = True
  eithRespStr <- H.sendRequest logger takesJSON request -- Either String a
  pure $ eithRespStr >>= parseUpdatesResponse

parseHTTPResponse :: BSL.ByteString -> Either String TlReply
parseHTTPResponse resp -- Either
  =
  do
    val <-
      maybe (Left "Couldn't parse HTTP response") Right $
        Ae.decode resp
    parseEither parseJSON val

sendThis ::
  L.LoggerHandler IO ->
  H.HTTPRequest ->
  IO (Either String TlReply)
sendThis logger request = do
  let takesJSON = tlTakesJSON
  eithRespStr <- H.sendRequest logger takesJSON request
  pure $ eithRespStr >>= parseHTTPResponse
