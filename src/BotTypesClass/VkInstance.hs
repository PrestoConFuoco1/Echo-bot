{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module BotTypesClass.VkInstance where

import BotTypesClass.ClassTypes
import qualified Messenger as M
import Vkontakte

instance BotClassTypes 'M.Vkontakte where
  type StateC 'M.Vkontakte = VkStateConst
  type StateM 'M.Vkontakte = VkStateMut
  type Rep 'M.Vkontakte = VkReply
  type RepErr 'M.Vkontakte = VkUpdateReplyError
  type RepSucc 'M.Vkontakte = VkUpdateReplySuccess
  type Upd 'M.Vkontakte = VkUpdate
  type Msg 'M.Vkontakte = VkMessage
  type Chat 'M.Vkontakte = VkChat
  type User 'M.Vkontakte = VkUser
  type CallbackQuery 'M.Vkontakte = VkMyCallback
  type Hndl 'M.Vkontakte = VkHandler
