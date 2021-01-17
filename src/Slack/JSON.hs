-------------------------------------------------------------------------------
-- Haskell bindings for the Slack APIs                                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Slack.JSON (
    module Data.Aeson,
    module Deriving.Aeson,
    SlackJSON,
    HasPayload(..)
) where

-------------------------------------------------------------------------------

import Data.Aeson
import Data.Aeson.Types
import Data.Text

import Deriving.Aeson

-------------------------------------------------------------------------------

-- | Represents type-level options that should be used to automatically derive
-- `ToJSON` and `FromJSON` instances.
type SlackJSON str = 
    CustomJSON '[ OmitNothingFields
                , FieldLabelModifier (StripPrefix str, CamelToSnake)
                ]

-- | A class of types that are contained as payload in Slack API responses.
class FromJSON a => HasPayload a where
    -- | `payloadFieldName` @proxy@ is payload field name. This can simply
    -- be set to the key for responses that contain a single payload field.
    payloadFieldName :: Text

    -- | `payload` @object@ parses @object@ into the payload. This can be
    -- overriden if a response contains more than one payload field.
    payload :: Object -> Parser a
    payload v = v .: payloadFieldName @a

-------------------------------------------------------------------------------
