-------------------------------------------------------------------------------
-- Haskell bindings for the Slack APIs                                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Slack.Types.Message (
    Reaction(..),
    SlackMessage(..)
) where

-------------------------------------------------------------------------------

import Data.Text

import Slack.JSON

-------------------------------------------------------------------------------

data Reaction = MkReaction {
    reactionName :: Text,
    reactionUsers :: [Text],
    reactionCount :: Int
} deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON)
  via SlackJSON "reaction" Reaction

data SlackMessage = MkSlackMessage {
    messageType :: Text,
    messageText :: Text,
    messageReactions :: [Reaction]
} deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON)
  via SlackJSON "message" SlackMessage

instance HasPayload SlackMessage where
    payloadFieldName = "message"

-------------------------------------------------------------------------------
