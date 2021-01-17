-------------------------------------------------------------------------------
-- Haskell bindings for the Slack APIs                                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Slack.API.Admin (
    AdminAPI
) where

-------------------------------------------------------------------------------

import Servant.API

import Slack.API.Auth

-------------------------------------------------------------------------------

type AdminUsersAPI
    = "admin.users.list" :>
      Post '[JSON] ()

type AdminAPI
    = AdminUsersAPI 

-------------------------------------------------------------------------------
