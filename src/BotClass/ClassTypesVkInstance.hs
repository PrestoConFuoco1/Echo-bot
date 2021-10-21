{-# LANGUAGE TypeFamilies #-}

module BotClass.ClassTypesVkInstance where

import BotClass.ClassTypes
import Vkontakte

data Vk =
   Vk

instance BotClassTypes Vk where
   type Conf Vk = VkConfig
   type StateC Vk = VkStateConst
   type StateM Vk = VkStateMut
   type Rep Vk = VkReply
   type RepErr Vk = VkUpdateReplyError
   type RepSucc Vk = VkUpdateReplySuccess
   type Upd Vk = VkUpdate
   type Msg Vk = VkMessage
   type Chat Vk = VkChat
   type User Vk = VkUser
   type CallbackQuery Vk = VkMyCallback
   type Hndl Vk = VkHandler
