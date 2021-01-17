-------------------------------------------------------------------------------
-- Haskell bindings for the Slack APIs                                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Slack.API.Reactions (
    ReactionsAPI,
    ReactionsGetReq(..)
) where

-------------------------------------------------------------------------------

import Data.Default.Class
import Data.Text

import Deriving.Aeson

import Servant.API

import Slack.JSON
import Slack.FormData
import Slack.Response
import Slack.API.Auth
import Slack.Types.Message

-------------------------------------------------------------------------------

data ReactionsGetReq = MkReactionsGetReq {
    reactionsChannel :: Maybe Text,
    reactionsFull :: Maybe Bool,
    reactionsTimestamp :: Maybe Text
} deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON)
  via SlackJSON "reactions" ReactionsGetReq

instance ToForm ReactionsGetReq where
    toForm MkReactionsGetReq{..} =
        optionalAttr "channel" reactionsChannel <>
        optionalAttr "full" reactionsFull <>
        optionalAttr "timestamp" reactionsTimestamp

instance Default ReactionsGetReq where
    def = MkReactionsGetReq{
        reactionsChannel = Nothing,
        reactionsFull = Nothing,
        reactionsTimestamp = Nothing
    }

type ReactionsAPI
    = SlackAuth :>
      "reactions.get" :>
      ReqBody '[FormUrlEncoded] ReactionsGetReq :>
      Post '[JSON] (SlackResponse SlackMessage)

-------------------------------------------------------------------------------
