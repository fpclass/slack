-------------------------------------------------------------------------------
-- Haskell bindings for the Slack APIs                                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Slack.Types.User (
    SlackProfile(..),
    SlackUser(..)
) where

-------------------------------------------------------------------------------

import Data.Text

import Slack.JSON

-------------------------------------------------------------------------------

-- | Represents users profiles.
data SlackProfile = MkSlackProfile {
    profileEmail :: Maybe Text,
    profileRealName :: Text
} deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON)
  via SlackJSON "profile" SlackProfile

-- | Represents Slack user accounts, including bots.
data SlackUser = MkSlackUser {
    userId :: Text,
    userName :: Text,
    userRealName :: Maybe Text,
    userProfile :: SlackProfile
} deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON)
  via SlackJSON "user" SlackUser

instance HasPayload [SlackUser] where
    payloadFieldName = "members"

-------------------------------------------------------------------------------
