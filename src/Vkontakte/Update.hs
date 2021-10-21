{-# LANGUAGE
    DeriveGeneric
    #-}
module Vkontakte.Update where

import Data.Aeson (decode)
import Data.Aeson.Types
import GHC.Generics
import Data.Foldable (asum)
import qualified Stuff as S (showTL)
import qualified Data.Text.Lazy.Encoding as EL (encodeUtf8)
import qualified Data.Text.Lazy as TL (Text)
import qualified Data.Text as T (Text)
import GenericPretty
import Types
import Vkontakte.Entity


data VkReply = VkReply {
--    _VR_ts :: Maybe TL.Text,
--    _VR_updates :: Maybe Value,
    _VR_failed :: Maybe Int
    } deriving (Eq, Show, Generic)

instance FromJSON VkReply where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 4 }

data VkUpdateReplySuccess = VkUpdateReplySuccess {
    _VURS_updates :: Value
    , _VURS_ts :: Maybe TL.Text
    } deriving (Show, Eq, Generic)

data VkUpdateReplyError = VkUpdateReplyError {
    _VURE_failed :: Int
    , _VURE_ts :: Maybe TL.Text
    } deriving (Show, Eq, Generic)

parseUpdatesResponse2 :: Value -> Either String (UpdateResponse VkUpdateReplySuccess VkUpdateReplyError)
parseUpdatesResponse2 = parseEither $ withObject "Vkontakte update object" $ \o -> do
    ts <- asum [
        o .:? "ts"
        , fmap (fmap S.showTL) $ (o .:? "ts" :: Parser (Maybe Integer))
        ]
    let success = do
            updates <- o .: "updates"
            return $ VkUpdateReplySuccess {
                _VURS_updates = updates
                , _VURS_ts = ts
                }
        err = do
            failed <- o .: "failed"
            return $ VkUpdateReplyError {
                _VURE_failed = failed
                , _VURE_ts = ts
                }
    asum [ fmap UpdateResponse success,
           fmap UpdateError err ]


----------------------------------------------

data VkUpdate = VkUpdate {
     _VU_value :: Value
    , _VU_object :: VkEvent
--    ,_VU_type :: T.Text
    } deriving (Eq, Show)

data VkEvent = VEMsg VkMessage
            -- | VEMsgEdit VkMessage
             | VECallback VkMyCallback
             | VEUnexpectedEvent
             deriving (Eq, Show)
instance FromJSON VkUpdate where
    parseJSON value = ($value) $ withObject "update object" $ \o -> do
        updType <- o .: "type" :: Parser T.Text
        event <- case updType of
-- API version >= 5.103 will be used
            "message_new"  -> do
-- if the field "payload" is present, it is an object and has "callback" field,
-- then it is callback update.
                q <- o .: "object"
                msg <- q .: "message" :: Parser Value
                asum [
                    fmap VECallback $ parseCallback msg,
                    fmap VEMsg $ parseJSON msg
                    ]
                
 --           "message_edit" -> fmap VEMsgEdit $ o .: "object"
 --           str -> fail $ T.unpack $ "Failed to parse the event object of type \"" <> str <> "\"."
            _ -> return VEUnexpectedEvent
        return $ VkUpdate
            --updType
            value event

parseCallback :: Value -> Parser VkMyCallback
parseCallback = withObject "Expected message object with payload" $ \msg -> do
    pt <- msg .: "payload" :: Parser TL.Text
    let pbs = {- S.echo $-} EL.encodeUtf8 pt
        pVal = {-S.echo $-} decode pbs :: Maybe Value
    payload <- maybe (fail "unable to parse payload object") return pVal >>= parseJSON
    text <- msg .: "text"
    from_id <- msg .: "from_id"
    return $ VkMyCallback from_id text payload


instance PrettyShow VkReply
instance PrettyShow VkUpdateReplyError
instance PrettyShow VkUpdateReplySuccess


