module Vkontakte.Send
   ( getUpdates
   , sendThis
   ) where

import qualified App.Logger as L
import qualified Data.Aeson as Ae (decode)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BSL
   ( ByteString
   )
import qualified HTTPRequests as H
import Types
import Vkontakte.General
import Vkontakte.Update

parseUpdatesResponse ::
      BSL.ByteString
   -> Either String (UpdateResponse VkUpdateReplySuccess VkUpdateReplyError)
parseUpdatesResponse resp -- Either
 = do
   val <-
      maybe
         (Left
             "Couldn't parse updates response into aeson Value")
         Right $
      Ae.decode resp
   parseUpdatesResponse2 val

getUpdates ::
      L.Handle IO
   -> H.HTTPRequest
   -> IO (Either String (UpdateResponse VkUpdateReplySuccess VkUpdateReplyError))
getUpdates logger request = do
   let takesJSON = True
   eithRespStr <- H.sendRequest logger takesJSON request -- Either String a
   let eithResp = eithRespStr >>= parseUpdatesResponse
   return eithResp

parseHTTPResponse :: BSL.ByteString -> Either String VkReply
parseHTTPResponse resp -- Either
 = do
   val <-
      maybe (Left "Couldn't parse HTTP response") Right $
      decode resp
   parseEither parseJSON val

sendThis ::
      L.Handle IO
   -> H.HTTPRequest
   -> IO (Either String VkReply)
sendThis logger request = do
   let takesJSON = vkTakesJSON
   eithRespStr <- H.sendRequest logger takesJSON request
   let eithResp = eithRespStr >>= parseHTTPResponse
   return eithResp
