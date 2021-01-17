-------------------------------------------------------------------------------
-- Haskell bindings for the Slack APIs                                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Slack.Response ( 
    ResponseMetadata(..),
    SlackResponse(..) 
) where

-------------------------------------------------------------------------------

import Data.Text

import Slack.JSON

-------------------------------------------------------------------------------

data ResponseMetadata = MkResponseMetadata {
    metaNextCursor :: Text
} deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON)
  via SlackJSON "meta" ResponseMetadata

data SlackResponse a 
    = Ok {
        responsePayload :: a,
        responseMetadata :: Maybe ResponseMetadata
    } 
    | NotOk 
    deriving (Eq, Show, Generic)

instance (HasPayload a, FromJSON a) => FromJSON (SlackResponse a) where
    parseJSON = withObject "SlackResponse" $ \obj -> do
        ok <- obj .: "ok"

        if ok
        then Ok <$> payload obj
                <*> obj .:? "response_metadata"
        else pure NotOk

-------------------------------------------------------------------------------
