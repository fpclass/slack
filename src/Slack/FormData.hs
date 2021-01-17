-------------------------------------------------------------------------------
-- Haskell bindings for the Slack APIs                                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Slack.FormData (
    module Web.FormUrlEncoded,
    optionalAttr
) where

-------------------------------------------------------------------------------

import Data.Text 

import Servant.API

import Web.FormUrlEncoded

-------------------------------------------------------------------------------

optionalAttr :: ToHttpApiData a => Text -> Maybe a -> Form
optionalAttr _    Nothing  = []
optionalAttr name (Just x) = [ (name, toQueryParam x) ]

-------------------------------------------------------------------------------
