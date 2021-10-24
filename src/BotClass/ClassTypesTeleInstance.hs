{-# LANGUAGE TypeFamilies, DataKinds #-}
module BotClass.ClassTypesTeleInstance where

import BotClass.ClassTypes
import Telegram
import Types

instance BotClassTypes 'Telegram where
   type Conf 'Telegram = TlConfig -- don't care
   type StateC 'Telegram = TlStateConst
   type StateM 'Telegram = TlStateMut
   type Rep 'Telegram = TlReply
   type RepErr 'Telegram = TlUpdateReplyError
   type RepSucc 'Telegram = TlUpdateReplySuccess
   type Upd 'Telegram = TlUpdate
   type Msg 'Telegram = TlMessage
   type Chat 'Telegram = TlChat
   type User 'Telegram = TlUser
   type CallbackQuery 'Telegram = TlCallback
   type Hndl 'Telegram = TlHandler
