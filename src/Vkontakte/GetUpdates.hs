module Vkontakte.GetUpdates where


import qualified HTTPRequests as H
import Types
import qualified App.Logger as L
import qualified Data.Aeson as Ae (decode)
import qualified Data.ByteString.Lazy.Char8 as BSL (ByteString)
import Vkontakte.Entity


parseUpdatesResponse :: BSL.ByteString
    -> Either String (UpdateResponse VkUpdateReplySuccess VkUpdateReplyError)
parseUpdatesResponse resp = do -- Either
    val <- maybe (Left "Couldn't parse updates response into aeson Value") Right $ Ae.decode resp
    repl <- parseUpdatesResponse2 val
    return repl


-- getUpdates :: H.HTTPRequest -> m (Either String (UpdateResponse (RepSucc s) (RepErr s))),
getUpdates :: L.Handle IO -> H.HTTPRequest -> IO (Either String (UpdateResponse VkUpdateReplySuccess VkUpdateReplyError))
getUpdates logger request = do
    let
        takesJSON = True
    eithRespStr <- H.sendRequest logger (takesJSON) request -- Either String a
    let eithResp = eithRespStr >>= parseUpdatesResponse
    return eithResp


