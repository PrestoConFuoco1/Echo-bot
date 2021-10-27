{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module BotClass.ClassTypesVkInstance where

import BotClass.ClassTypes
import qualified Types as Y
import Vkontakte

instance BotClassTypes 'Y.Vkontakte where
  type StateC 'Y.Vkontakte = VkStateConst
  type StateM 'Y.Vkontakte = VkStateMut
  type Rep 'Y.Vkontakte = VkReply
  type RepErr 'Y.Vkontakte = VkUpdateReplyError
  type RepSucc 'Y.Vkontakte = VkUpdateReplySuccess
  type Upd 'Y.Vkontakte = VkUpdate
  type Msg 'Y.Vkontakte = VkMessage
  type Chat 'Y.Vkontakte = VkChat
  type User 'Y.Vkontakte = VkUser
  type CallbackQuery 'Y.Vkontakte = VkMyCallback
  type Hndl 'Y.Vkontakte = VkHandler
