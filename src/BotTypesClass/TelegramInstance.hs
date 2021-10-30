{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module BotTypesClass.TelegramInstance
    (
    ) where

import BotTypesClass.ClassTypes (BotClassTypes(..))
import qualified Messenger as M
import Telegram

instance BotClassTypes 'M.Telegram where
    type Rep 'M.Telegram = TlReply
    type RepErr 'M.Telegram = TlUpdateReplyError
    type RepSucc 'M.Telegram = TlUpdateReplySuccess
    type Upd 'M.Telegram = TlUpdate
    type Msg 'M.Telegram = TlMessage
    type Chat 'M.Telegram = TlChat
    type User 'M.Telegram = TlUser
    type CallbackQuery 'M.Telegram = TlCallback
