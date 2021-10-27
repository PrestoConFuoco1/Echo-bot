{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module BotTypesClass.ClassTypesTeleInstance where

import BotTypesClass.ClassTypes (BotClassTypes (..))
import Telegram
import qualified Types as Y

instance BotClassTypes 'Y.Telegram where
  type StateC 'Y.Telegram = TlStateConst
  type StateM 'Y.Telegram = TlStateMut
  type Rep 'Y.Telegram = TlReply
  type RepErr 'Y.Telegram = TlUpdateReplyError
  type RepSucc 'Y.Telegram = TlUpdateReplySuccess
  type Upd 'Y.Telegram = TlUpdate
  type Msg 'Y.Telegram = TlMessage
  type Chat 'Y.Telegram = TlChat
  type User 'Y.Telegram = TlUser
  type CallbackQuery 'Y.Telegram = TlCallback
  type Hndl 'Y.Telegram = TlHandler
