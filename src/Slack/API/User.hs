-------------------------------------------------------------------------------
-- Haskell bindings for the Slack APIs                                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Slack.API.User (
    UsersAPI,
    UsersListReq(..)
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
import Slack.Types.User

-------------------------------------------------------------------------------

data UsersListReq = MkUsersListReq {
    reqCursor :: Maybe Text,
    reqLimit :: Maybe Int
} deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON)
  via SlackJSON "req" UsersListReq

instance ToForm UsersListReq where
    toForm MkUsersListReq{..} =
        optionalAttr "limit" reqLimit <>
        optionalAttr "cursor" reqCursor

instance Default UsersListReq where
    def = MkUsersListReq{
        reqCursor = Nothing,
        reqLimit = Nothing
    }

type UsersAPI
    = SlackAuth :>
      "users.list" :>
      ReqBody '[FormUrlEncoded] UsersListReq :>
      Post '[JSON] (SlackResponse [SlackUser])

-------------------------------------------------------------------------------
