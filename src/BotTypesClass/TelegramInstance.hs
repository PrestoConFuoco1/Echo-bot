{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module BotTypesClass.TelegramInstance where

import BotTypesClass.ClassTypes (BotClassTypes (..))
import Telegram
import qualified Messenger as M

instance BotClassTypes 'M.Telegram where
  type StateC 'M.Telegram = TlStateConst
  type StateM 'M.Telegram = TlStateMut
  type Rep 'M.Telegram = TlReply
  type RepErr 'M.Telegram = TlUpdateReplyError
  type RepSucc 'M.Telegram = TlUpdateReplySuccess
  type Upd 'M.Telegram = TlUpdate
  type Msg 'M.Telegram = TlMessage
  type Chat 'M.Telegram = TlChat
  type User 'M.Telegram = TlUser
  type CallbackQuery 'M.Telegram = TlCallback
  type Hndl 'M.Telegram = TlHandler
