-------------------------------------------------------------------------------
-- Haskell bindings for the Slack APIs                                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Slack.API (
    SlackAPI,
    slackApi
) where

-------------------------------------------------------------------------------

import Data.Proxy

import Servant.API
import Servant.Client

import Slack.API.Admin

-------------------------------------------------------------------------------

-- | The Slack API as a type.
type SlackAPI
    = AdminAPI

-- | A `Proxy` for `SlackAPI`.
slackApi :: Proxy SlackAPI
slackApi = Proxy

-------------------------------------------------------------------------------

adminListUsers = client slackApi

-------------------------------------------------------------------------------
