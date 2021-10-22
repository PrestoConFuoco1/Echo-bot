module Telegram.Send
   ( getUpdates
   , sendThis
   ) where

import qualified App.Logger as L
import qualified Data.Aeson as Ae (decode)
import Data.Aeson.Types as AeT
import qualified Data.ByteString.Lazy.Char8 as BSL
   ( ByteString
   )
import qualified HTTPRequests as H
import Telegram.General (tlTakesJSON)
import Telegram.Update
import Types

parseUpdatesResponse ::
      BSL.ByteString
   -> Either String (UpdateResponse TlUpdateReplySuccess TlUpdateReplyError)
parseUpdatesResponse resp -- Either
 = do
   val <-
      maybe
         (Left
             "Couldn't parse updates response into aeson Value")
         Right $
      Ae.decode resp
   parseUpdatesResponse1 val

getUpdates ::
      L.Handle IO
   -> H.HTTPRequest
   -> IO (Either String (UpdateResponse TlUpdateReplySuccess TlUpdateReplyError))
getUpdates logger request = do
   let takesJSON = True
   eithRespStr <- H.sendRequest logger takesJSON request -- Either String a
   let eithResp = eithRespStr >>= parseUpdatesResponse
   return eithResp

parseHTTPResponse :: BSL.ByteString -> Either String TlReply
parseHTTPResponse resp -- Either
 = do
   val <-
      maybe (Left "Couldn't parse HTTP response") Right $
      Ae.decode resp
   parseEither parseJSON val

sendThis ::
      L.Handle IO
   -> H.HTTPRequest
   -> IO (Either String TlReply)
sendThis logger request = do
   let takesJSON = tlTakesJSON
   eithRespStr <- H.sendRequest logger takesJSON request
   let eithResp = eithRespStr >>= parseHTTPResponse
   return eithResp
