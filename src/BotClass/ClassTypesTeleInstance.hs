{-# LANGUAGE TypeFamilies #-}
module BotClass.ClassTypesTeleInstance where

import BotClass.ClassTypes
import Telegram

data Tele = Tele
dummyTl = Tele



instance BotClassTypes Tele where
    type Conf Tele = TlConfig -- don't care

    type StateC Tele = TlStateConst
    type StateM Tele = TlStateMut
    type Rep Tele = TlReply
    type RepErr Tele = TlUpdateReplyError
    type RepSucc Tele = TlUpdateReplySuccess
    type Upd Tele = TlUpdate
    type Msg Tele = TlMessage
    type Chat Tele = TlChat
    type User Tele = TlUser

    type CallbackQuery Tele = TlCallback

    type Hndl Tele = TlHandler

