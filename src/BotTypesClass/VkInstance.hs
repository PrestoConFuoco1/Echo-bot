{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module BotTypesClass.VkInstance () where

import BotTypesClass.ClassTypes (BotClassTypes (..))
import qualified Messenger as M
import Vkontakte

instance BotClassTypes 'M.Vkontakte where
  type Rep 'M.Vkontakte = VkReply
  type RepErr 'M.Vkontakte = VkUpdateReplyError
  type RepSucc 'M.Vkontakte = VkUpdateReplySuccess
  type Upd 'M.Vkontakte = VkUpdate
  type Msg 'M.Vkontakte = VkMessage
  type Chat 'M.Vkontakte = VkChat
  type User 'M.Vkontakte = VkUser
  type CallbackQuery 'M.Vkontakte = VkMyCallback
