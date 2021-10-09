{-# LANGUAGE TypeFamilies #-}
module BotClass.ClassTypesTeleInstance where

import Telegram.Types
import BotClass.ClassTypes

instance BotClassTypes Tele where
    type Conf Tele = TlConfig -- don't care

    type StateC Tele = TlStateConst
    type StateM Tele = TlStateMut
    type Rep Tele = TlReply
    type Upd Tele = TlUpdate
    type Msg Tele = TlMessage
    type Chat Tele = TlChat
    type User Tele = TlUser

    type CallbackQuery Tele = TlCallback

    type Hndl Tele = TlHandler

