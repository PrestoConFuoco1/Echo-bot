{-# LANGUAGE
    DeriveGeneric
    #-}
module Telegram.MediaGroup where

import GHC.Generics
import qualified GenericPretty as GP
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Aeson (encode)
import Telegram.Entity
import qualified App.Handle as D
--import App.Handle.Telegram
import qualified Stuff as S (safeHead, withMaybe)
import Telegram.ProcessMessage.Types
import Data.Foldable (asum)
import Telegram.MediaGroup.Types
import BotClass.ClassTypesTeleInstance
import Telegram.ForHandlers

processMediaGroup :: (Monad m) => D.Handle Tele m -> TlMessage -> m ()
processMediaGroup h m = let
    chat = _TM_chat m
    mMediaGroupID = _TM_media_group_id m
    mPhoto = _TM_photo m >>= S.safeHead :: Maybe TlPhotoSize
    mMediaGroupIdent = TlMediaGroupIdentifier chat <$> mMediaGroupID
    mAction = asum [ insertMediaGroupPhoto (D.specH h) <$> mMediaGroupIdent <*> mPhoto ]
    in S.withMaybe mAction (return ()) (\a -> D.logDebug h "Processing media group" >> a)


