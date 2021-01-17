-------------------------------------------------------------------------------
-- Haskell bindings for the Slack APIs                                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Slack.API.Auth ( 
    SlackAuth,
    authenticateReq
) where

-------------------------------------------------------------------------------

import Data.Text

import Servant.API (AuthProtect)
import Servant.Client.Core

-------------------------------------------------------------------------------

type SlackAuth = AuthProtect "token"

type instance AuthClientData (AuthProtect "token") = Text

-- | `authenticateReq` @token req@ adds the @token@ to the headers of @req@.
authenticateReq :: Text -> Request -> Request
authenticateReq token = addHeader "Authorization" ("Bearer " <> token)

-------------------------------------------------------------------------------
