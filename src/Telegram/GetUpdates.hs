module Telegram.GetUpdates where

import Telegram.Entity
import qualified HTTPRequests as H
import Types
import qualified App.Logger as L
import Telegram.Entity
import qualified Data.Aeson as Ae (decode)
import qualified Data.ByteString.Lazy.Char8 as BSL (ByteString)

parseUpdatesResponse :: BSL.ByteString
    -> Either String (UpdateResponse TlUpdateReplySuccess TlUpdateReplyError)
parseUpdatesResponse resp = do -- Either
    val <- maybe (Left "Couldn't parse updates response into aeson Value") Right $ Ae.decode resp
    repl <- parseUpdatesResponse1 val
    return repl


-- getUpdates :: H.HTTPRequest -> m (Either String (UpdateResponse (RepSucc s) (RepErr s))),
getUpdates :: L.Handle IO -> H.HTTPRequest -> IO (Either String (UpdateResponse TlUpdateReplySuccess TlUpdateReplyError))
getUpdates logger request = do
    let
        takesJSON = True
    eithRespStr <- H.sendRequest logger (takesJSON) request -- Either String a
    let eithResp = eithRespStr >>= parseUpdatesResponse
    return eithResp


