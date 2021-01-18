-------------------------------------------------------------------------------
-- Haskell bindings for the Slack APIs                                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Slack.API.Chat (
    ChatAPI,
    ChatPostMessageReq(..)
) where

-------------------------------------------------------------------------------

import Data.Text

import Deriving.Aeson

import Servant.API

import Slack.JSON
import Slack.Response
import Slack.API.Auth
import Slack.Types.Message

-------------------------------------------------------------------------------

data ChatPostMessageReq = MkChatPostMessageReq {
    postMessageChannel :: Text,
    postMessageText :: Text
} deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON)
  via SlackJSON "postMessage" ChatPostMessageReq

type ChatAPI
    = SlackAuth :>
      "chat.postMessage" :>
      ReqBody '[JSON] ChatPostMessageReq :>
      Post '[JSON] (SlackResponse SlackMessage)

-------------------------------------------------------------------------------
