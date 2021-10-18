module Telegram.Send (getUpdates, sendThis) where

import qualified HTTPRequests as H
import Types
import qualified App.Logger as L
import Telegram.Entity
import qualified Data.Aeson as Ae (decode)
import qualified Data.ByteString.Lazy.Char8 as BSL (ByteString)
import Data.Aeson.Types as AeT
import Telegram.ForHandlers
import Telegram.Update
import Telegram.General

parseUpdatesResponse :: BSL.ByteString
    -> Either String (UpdateResponse TlUpdateReplySuccess TlUpdateReplyError)
parseUpdatesResponse resp = do -- Either
    val <- maybe (Left "Couldn't parse updates response into aeson Value") Right $ Ae.decode resp
    repl <- parseUpdatesResponse1 val
    return repl


getUpdates :: L.Handle IO -> H.HTTPRequest -> IO (Either String (UpdateResponse TlUpdateReplySuccess TlUpdateReplyError))
getUpdates logger request = do
    let
        takesJSON = True
    eithRespStr <- H.sendRequest logger (takesJSON) request -- Either String a
    let eithResp = eithRespStr >>= parseUpdatesResponse
    return eithResp

parseHTTPResponse :: BSL.ByteString -> Either String TlReply
parseHTTPResponse resp = do -- Either
    val <- maybe (Left "Couldn't parse HTTP response") Right $ Ae.decode resp
    repl <- parseEither parseJSON val
    return repl


sendThis :: L.Handle IO -> H.HTTPRequest -> IO (Either String TlReply)
sendThis logger request = do
    let takesJSON = tlTakesJSON
    eithRespStr <- H.sendRequest logger (takesJSON) request
    let eithResp = eithRespStr >>= parseHTTPResponse
    return eithResp



sendHelp :: L.Handle IO -> H.HTTPRequest -> IO (Either String TlReply)
sendHelp = sendThis
