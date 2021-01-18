-------------------------------------------------------------------------------
-- Haskell bindings for the Slack APIs                                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Slack.API (
    SlackAPI,
    slackApi,
    Slack,
    -- * Users API
    listUsers,
    -- * Reactions API
    getReactions,
    -- * Chat API
    postMessage
) where

-------------------------------------------------------------------------------

import Control.Monad.Reader

import Data.Text
import Data.Proxy

import Servant.API
import Servant.Client
import Servant.Client.Core 

import Slack.Response
import Slack.API.Auth
import Slack.API.Chat
import Slack.API.User
import Slack.API.Reactions
import Slack.Types.Message
import Slack.Types.User

-------------------------------------------------------------------------------

type Slack = ReaderT Text ClientM

-------------------------------------------------------------------------------

-- | The Slack API as a type.
type SlackAPI
    = UsersAPI
 :<|> ReactionsAPI
 :<|> ChatAPI

-- | A `Proxy` for `SlackAPI`.
slackApi :: Proxy SlackAPI
slackApi = Proxy

-------------------------------------------------------------------------------

userApi :<|> reactionsApi :<|> chatApi = client slackApi

_listUsers = userApi

_getReactions = reactionsApi

_postMessage = chatApi

-------------------------------------------------------------------------------

withToken 
    :: (AuthenticatedRequest (AuthProtect "token") -> ClientM a) 
    -> Slack a
withToken cont = do 
    token <- flip mkAuthenticatedRequest authenticateReq <$> ask
    lift $ cont token

-- | `listUsers` @req@ retrieves users in the workspace.
listUsers :: UsersListReq -> Slack (SlackResponse [SlackUser])
listUsers req = withToken $ \token -> _listUsers token req

-- | `getReactions` @req@ retrieves a `SlackMessage`, including information
-- about which users reacted to it.
getReactions :: ReactionsGetReq -> Slack (SlackResponse SlackMessage)
getReactions req = withToken $ \token -> _getReactions token req

-- | `postMessage` @req@ posts a message to a conversation and returns
-- information about the message that was created.
postMessage :: ChatPostMessageReq -> Slack (SlackResponse SlackMessage)
postMessage req = withToken $ \token -> _postMessage token req

-------------------------------------------------------------------------------
