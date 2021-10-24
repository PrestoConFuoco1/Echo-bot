{-# LANGUAGE TypeFamilies, DataKinds #-}
module BotClass.ClassTypesVkInstance where

import BotClass.ClassTypes
import Vkontakte
import Types

instance BotClassTypes 'Vkontakte where
   type Conf 'Vkontakte = VkConfig
   type StateC 'Vkontakte = VkStateConst
   type StateM 'Vkontakte = VkStateMut
   type Rep 'Vkontakte = VkReply
   type RepErr 'Vkontakte = VkUpdateReplyError
   type RepSucc 'Vkontakte = VkUpdateReplySuccess
   type Upd 'Vkontakte = VkUpdate
   type Msg 'Vkontakte = VkMessage
   type Chat 'Vkontakte = VkChat
   type User 'Vkontakte = VkUser
   type CallbackQuery 'Vkontakte = VkMyCallback
   type Hndl 'Vkontakte = VkHandler
