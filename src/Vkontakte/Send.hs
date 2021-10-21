module Vkontakte.Send (getUpdates, sendThis) where


import qualified HTTPRequests as H
import Types
import qualified App.Logger as L
import qualified Data.Aeson as Ae (decode)
import qualified Data.ByteString.Lazy.Char8 as BSL (ByteString)
import Vkontakte.General
import Vkontakte.Update
import Data.Aeson
import Data.Aeson.Types

parseUpdatesResponse :: BSL.ByteString
    -> Either String (UpdateResponse VkUpdateReplySuccess VkUpdateReplyError)
parseUpdatesResponse resp = do -- Either
    val <- maybe (Left "Couldn't parse updates response into aeson Value") Right $ Ae.decode resp
    repl <- parseUpdatesResponse2 val
    return repl


getUpdates :: L.Handle IO -> H.HTTPRequest -> IO (Either String (UpdateResponse VkUpdateReplySuccess VkUpdateReplyError))
getUpdates logger request = do
    let
        takesJSON = True
    eithRespStr <- H.sendRequest logger (takesJSON) request -- Either String a
    let eithResp = eithRespStr >>= parseUpdatesResponse
    return eithResp


parseHTTPResponse :: BSL.ByteString -> Either String VkReply
parseHTTPResponse resp = do -- Either
    val <- maybe (Left "Couldn't parse HTTP response") Right $ decode resp
    repl <- parseEither parseJSON val
    return repl



sendThis :: L.Handle IO -> H.HTTPRequest -> IO (Either String VkReply)
sendThis logger request = do
    let takesJSON = vkTakesJSON
    eithRespStr <- H.sendRequest logger (takesJSON) request
    let eithResp = eithRespStr >>= parseHTTPResponse
    return eithResp



