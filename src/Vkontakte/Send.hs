module Vkontakte.Send
  ( getUpdates,
    sendThis,
  )
where

import qualified App.Logger as L
import Data.Aeson (decode)
import qualified Data.Aeson as Ae (decode)
import Data.Aeson.Types (parseEither, parseJSON)
import qualified Data.ByteString.Lazy.Char8 as BSL
  ( ByteString,
  )
import qualified HTTPTypes as H
import qualified HTTPSend as H
import qualified Types as Y
import Vkontakte.General (vkTakesJSON)
import Vkontakte.Update (VkReply, VkUpdateReplyError, VkUpdateReplySuccess, parseUpdatesResponse2)

parseUpdatesResponse ::
  BSL.ByteString ->
  Either String (Y.UpdateResponse VkUpdateReplySuccess VkUpdateReplyError)
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
    parseUpdatesResponse2 val

getUpdates ::
  L.LoggerHandler IO ->
  H.HTTPRequest ->
  IO (Either String (Y.UpdateResponse VkUpdateReplySuccess VkUpdateReplyError))
getUpdates logger request = do
  let takesJSON = True
  eithRespStr <- H.sendRequest logger takesJSON request -- Either String a
  pure $ eithRespStr >>= parseUpdatesResponse

parseHTTPResponse :: BSL.ByteString -> Either String VkReply
parseHTTPResponse resp -- Either
  =
  do
    val <-
      maybe (Left "Couldn't parse HTTP response") Right $
        decode resp
    parseEither parseJSON val

sendThis ::
  L.LoggerHandler IO ->
  H.HTTPRequest ->
  IO (Either String VkReply)
sendThis logger request = do
  let takesJSON = vkTakesJSON
  eithRespStr <- H.sendRequest logger takesJSON request
  pure $ eithRespStr >>= parseHTTPResponse
