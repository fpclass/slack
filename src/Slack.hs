-------------------------------------------------------------------------------
-- Haskell bindings for the Slack APIs                                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Slack (
    module Slack.API,
    SlackConfig(..),
    withSlackClient
) where 

-------------------------------------------------------------------------------

import Control.Monad.Reader

import Data.Text

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Servant.Client

import Slack.API

-------------------------------------------------------------------------------

data SlackConfig = MkSlackConfig {
    slackConfigToken :: Text
} deriving (Eq, Show)

slackApiUrl :: BaseUrl
slackApiUrl = BaseUrl Https "slack.com" 443 "/api"

withSlackClient :: Show a => SlackConfig -> Slack a -> IO (Either ClientError a)
withSlackClient MkSlackConfig{..} cont = do
    manager <- newManager tlsManagerSettings
    let env = ClientEnv manager slackApiUrl Nothing
    r <- runClientM (runReaderT cont slackConfigToken) env
    pure r

-------------------------------------------------------------------------------
